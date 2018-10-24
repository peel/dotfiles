{ config, pkgs, ... }:

{
  imports = [ ./fish.nix ];
  time.timeZone = "Europe/Warsaw";
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.emacs.enable = true;
  services.emacs.package = pkgs.emacs;
  environment.variables.EDITOR = "${pkgs.emacs}/bin/emacsclient -tc";
  environment.variables.SHELL = "${pkgs.fish}/bin/fish";
  environment.etc."editorconfig".text = ''
    # top-most EditorConfig file
    root = true

    [*]
    end_of_line = lf
    insert_final_newline = true
    trim_trailing_whitespace = true
    indent_style = space
    indent_size = 2
    charset = utf-8
  '';
  environment.etc."gitignore".text = ''
    ### Tags ###
    # Ignore tags created by etags, ctags, gtags (GNU global) and cscope
    TAGS
    .TAGS
    !TAGS/
    tags
    .tags
    !tags/
    gtags.files
    GTAGS
    GRTAGS
    GPATH
    GSYMS
    cscope.files
    cscope.out
    cscope.in.out
    cscope.po.out

    ### Ensime ###
    # Ensime specific
    .ensime
    .ensime_cache/
    .ensime_lucene/
  '';
  environment.etc."gitconfig".text = ''
    [include]
      path = ~/.gitconfig.secret
    [color]
      ui = true
    [color "branch"]
      current = yellow reverse
      local = yellow
      remote = green
    [color "diff"]
      meta = yellow bold
      frag = magenta bold
      old = red
      new = green
    [format]
      pretty = format:%C(blue)%ad%Creset %C(yellow)%h%C(green)%d%Creset %C(blue)%s %C(magenta) [%an]%Creset
    [mergetool]
      prompt = false
    [mergetool "mvimdiff"]
      cmd="mvim -c 'Gdiff' $MERGED"     # use fugitive.vim for 3-way merge
      keepbackup=false
    [merge]
      summary = true
      verbosity = 1
      tool = mvimdiff
      ff = only
      conflictstyle = diff3
    [apply]
      whitespace = nowarn
    [branch]
      autosetupmerge = true
    [push]
      # 'git push' will push the current branch to its tracking branch
      # the usual default is to push all branches
      default = upstream
    [core]
      autocrlf = false
    [advice]
      statusHints = false
    [diff]
      # Git diff will use (i)ndex, (w)ork tree, (c)ommit and (o)bject
      # instead of a/b/c/d as prefixes for patches
      mnemonicprefix = true
      algorithm = patience
    [url "git@github.com:"]
      insteadOf = "gh:"
      pushInsteadOf = "github:"
      pushInsteadOf = "git://github.com/"
    [url "git://github.com/"]
      insteadOf = "github:"
    [url "git@gist.github.com:"]
      insteadOf = "gst:"
      pushInsteadOf = "gist:"
      pushInsteadOf = "git://gist.github.com/"
    [url "git://gist.github.com/"]
      insteadOf = "gist:"
  '';
  environment.etc."vimrc".text = ''
    set nonumber
    set relativenumber
    colorscheme default
  '';
  system.activationScripts.extraUserActivation.text = ''
    ln -sfn /etc/static/gitconfig $HOME/.gitconfig
    ln -sfn /etc/static/gitignore $HOME/.gitignore
    ln -sfn /etc/static/vimrc $HOME/.vimrc
  '';
  environment.shellAliases = {
    cx = "chmod +x";
    c = "clear";
    cls = "clear;ls";

    psa = "ps aux";
    pasg = "psa | grep";
    k9 = "kill -9";

    pbc = "pbcopy";
    pbp = "pbpaste";

    #docker
    dc = "docker-compose";
    d = "docker";
    # navigation
    o = "open";
    ":q" = "exit";
    ".." = "cd ..";
    "..." = "cd ../..";
    "...." = "cd ../../..";
    "....." = "cd ../../../..";
    # browsing;
    less = "less -R";
    tailf = "tail -f";
    ls = "ls -Gh";
    ll = "ls -al";
    # disk;
    df = "df -h";
    du = "du -h -d 2";
    # git
    git = "hub";
    gs = "git status";
    gci = "git ci";
    gco = "git co";
    gl = "git log --pretty --graph";
    # kubernetes
    kk = "kubectl config use-context";
    kc = "kubectl config current-context";
    # nix
    ne = "nix-env";
    neg = "ne -qaP | grep";
    ns = "nix-shell";
    nr = (if pkgs.stdenv.isDarwin then "darwin-rebuild" else "sudo nixos-rebuild");
    # apps
    dotfiles = "sh $HOME/wrk/dotfiles/result/bin/dotfiles";
    zenity = "${pkgs.qarma}/bin/qarma";
    vim = "${pkgs.emacs}/bin/emacsclient -nw";
    r = "${pkgs.ranger}/bin/ranger";
    grep = "${pkgs.ripgrep}/bin/rg";
    alacritty = "${pkgs.alacritty}/bin/alacritty -e ${pkgs.tmux}/bin/tmux -2 new-session -A -s main";
    qmk = ''${pkgs.scripts}/bin/qmk $HOME/wrk/qmk_firmware/layouts/community/ortho_4x12/peel/keymap.c'';
  };
  # environment.interactiveShellInit = ''
  #   eval "$(${pkgs.fasd}/bin/fasd --init auto)"
  # '';
  # programs.bash.enableCompletion = true;
  programs.tmux.enable = true;
}
