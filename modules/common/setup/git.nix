{ config, pkgs, lib, ... }:

with lib;

let
  gitignore = pkgs.writeText "gitignore.global" ''
    *~
    .Trash-*
    .nfs*
    .DS_Store
    ._*
    .Spotlight-V100
    .Trashes
    \#*\#
    *.elc
    tramp
    .\#*
    result/
    .projectile
    .dir-locals.el
    .envrc
    .bloop/
    .metals/
 '';
  gitConfig = {
    user = {
      email = "peel@users.noreply.github.com";
      name = "Piotr Limanowski";
    };
    github.user = "peel";
    ghi.token = "!security find-internet-password -a peel -s github.com -l 'ghi token' -w";
    use.signingkey = "peel@users.noreply.github.com";
    commit.gpgsign = false;
    color.ui = true;
    format.pretty = "format:%C(blue)%ad%Creset %C(yellow)%h%C(green)%d%Creset %C(blue)%s %C(magenta) [%an]%Creset";
    mergetool.prompt = false;
    merge = {
      summary = true;
      verbosity = 1;
      ff = "only";
      conflictstyle = "diff3";
    };
    diff = {
      mnemonicPrefix = true;
      algorithm = "patience";
      tool = "difftastic";
    };
    difftool = {
      prompt = false;
    };
    "difftool \"difftastic\"" = {
      cmd = ''difft "$LOCAL" "$REMOTE"'';
    };
    pager = { difftool = true; };
    apply.whitespace = "nowarn";
    branch.autosetuprebase = "always";
    push.default = "upstream";
    core = {
      autocrlf = false;
      excludefile = "${gitignore}";
    };
    advice.statusHints = false;
  };
in {
  environment.systemPackages =  [
    pkgs.git
    pkgs.gitAndTools.git-crypt
    pkgs.difftastic
  ];
  
  environment.shellAliases = {
    gs = "git status";
    gci = "git ci";
    gco = "git co";
    gl = "git log --pretty --graph";
  };
  
  environment.etc."gitconfig".text = generators.toINI {} gitConfig;

}
