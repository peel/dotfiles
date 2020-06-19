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
      email = "brandon@orther.dev";
      name = "Brandon Orther";
    };
    github.user = "orther";
    ghi.token = "!security find-internet-password -a orther -s github.com -l 'ghi token' -w";
    use.signingkey = "brandon@orther.dev";
    commit.gpgsign = true;
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
    };
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
  environment.systemPackages = with pkgs; [
    git
    gitAndTools.git-crypt
  ];
  
  environment.shellAliases = {
    gs = "git status";
    gci = "git ci";
    gco = "git co";
    gl = "git log --pretty --graph";
  };
  
  environment.etc."gitconfig".text = generators.toINI {} gitConfig;

}
