{ pkgs, lib, ... }:

let
  fishFasd = pkgs.stdenv.mkDerivation rec {
    name = "fishFasd";
    unpackPhase = ":";
    installPhase = ''
      mkdir -p $out/share/fish/vendor_completions.d
      cp $src/completions/*.fish "$out/share/fish/vendor_completions.d/"
      mkdir -p $out/share/fish/vendor_conf.d
      cp $src/conf.d/*.fish "$out/share/fish/vendor_conf.d/"
      mkdir -p $out/share/fish/vendor_functions.d
      cp $src/functions/*.fish "$out/share/fish/vendor_functions.d/"
    '';
    src = pkgs.fetchFromGitHub {
      owner = "fishgretel";
      repo = "fasd";
      rev = "a0a3c3503961b8cd36e6bec8a7ae0edbca19d105";
      sha256 = "0w618la3ggri1z2mlkkxczrr22xih6c795jgmf2634v2c1349cjq";
    };
  };
in {
  environment.systemPackages = with pkgs; [ fish fishFasd ];
  environment.etc."fish/functions/fish_prompt.fish".text = ''
    function _pwd_with_tilde
    set -l parent (basename (dirname $PWD))
    set -l current (basename $PWD)
    echo "$parent/$current "
    end

    function _in_git_directory
    git rev-parse --git-dir > /dev/null 2>&1
    end

    function _git_branch_name_or_revision
    set -l branch (git symbolic-ref HEAD ^ /dev/null | sed -e 's|^refs/heads/||')
    set -l revision (git rev-parse HEAD ^ /dev/null | cut -b 1-7)

    if test (count $branch) -gt 0
    echo $branch
    else
    echo $revision
    end
    end

    function _git_upstream_configured
    git rev-parse --abbrev-ref @"{u}" > /dev/null 2>&1
    end

    function _git_behind_upstream
    test (git rev-list --right-only --count HEAD...@"{u}" ^ /dev/null) -gt 0
    end

    function _git_ahead_of_upstream
    test (git rev-list --left-only --count HEAD...@"{u}" ^ /dev/null) -gt 0
    end

    function _git_upstream_status
    set -l arrows

    if _git_upstream_configured
    if _git_behind_upstream
    set arrows "$arrows⇣"
    end

    if _git_ahead_of_upstream
    set arrows "$arrows⇡"
    end
    end

    echo $arrows
    end

    function _print_in_color
    set -l string $argv[1]
    set -l color  $argv[2]

    set_color $color
    printf $string
    set_color normal
    end

    function _prompt_color_for_status
    if test $argv[1] -eq 0
    echo magenta
    else
    echo red
    end
    end

    function fish_prompt
    set -l last_status $status

    _print_in_color (_pwd_with_tilde) blue

    if _in_git_directory
    _print_in_color " "(_git_branch_name_or_revision) 242
    _print_in_color " "(_git_upstream_status) cyan
    end

    _print_in_color "λ " (_prompt_color_for_status $last_status)
    if set -q TMUX; tmux setenv TMUXPWD_(tmux display -p "#D" | tr -d '%') $PWD; end
    end
  '';
  programs.fish = {
    enable = true;
    vendor.completions.enable = true;
    promptInit = ''
      source /etc/fish/functions/fish_prompt.fish
    '';
  };

}
