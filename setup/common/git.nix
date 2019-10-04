{ config, pkgs, ... }:

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
  gitConfig = ''
    [user]
      email = piotr@codearsonist.com
      name = Piotr Limanowski
    [github]
      user = peel
    [ghi]
      token = !security find-internet-password -a peel -s github.com -l 'ghi token' -w
    [use]
      signingkey = piotr@codearsonist.com
    [commit]
      gpgsign = true
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
      excludefile = ${gitignore}
    [advice]
      statusHints = false
    [diff]
      # Git diff will use (i)ndex, (w)ork tree, (c)ommit and (o)bject
      # instead of a/b/c/d as prefixes for patches
      mnemonicprefix = true
      algorithm = patience
  '';
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
  
  environment.etc."gitconfig".text = gitConfig;

}
