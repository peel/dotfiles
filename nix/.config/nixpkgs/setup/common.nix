{ config, pkgs, ... }:

{
  time.timeZone = "Europe/Warsaw";
  
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      pragmatapro
    ];
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  
  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };

  environment.variables.EDITOR = "emacsclient -tc";
  environment.variables.ALTERNATE_EDITOR = "emacs";
  
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

  environment.shells = [ pkgs.bashInteractive ];
  environment.variables.SHELL = "/run/current-system/sw/bin/bash";
  programs.bash = {
    enable = true;
    enableCompletion = true;
    interactiveShellInit = ''
      shopt -s checkwinsize # track terminal window resize
      shopt -s extglob      # extended globbing capabilities
      shopt -s cdspell      # fix minor typos when cd'ing
      shopt -s cmdhist      # preserve new lines in history
      if [[ $BASH_VERSION == 4* ]] ; then
        shopt -s autocd       # type 'dir' instead 'cd dir'
        shopt -s dirspell     # correct typos when tab-completing names
        shopt -s globstar     # enable **
      fi
      PS1='\W$(__git_ps1 " - %s") λ '
    '';
  };
  environment.shellAliases = {
    k9 = "kill -9";
    pbc = "pbcopy";
    pbp = "pbpaste";
    dc = "docker-compose";
    d = "docker";
    o = "open";
    less = "less -R";
    tailf = "tail -f";
    ls = "ls -Gh";
    ll = "ls -al";
    df = "df -h";
    du = "du -h -d 2";
    gs = "git status";
    gci = "git ci";
    gco = "git co";
    gl = "git log --pretty --graph";
    kk = "kubectl config use-context";
    kc = "kubectl config current-context";
    nr = (if pkgs.stdenv.isDarwin then "darwin-rebuild" else "sudo nixos-rebuild");
    vim = "${pkgs.emacs}/bin/emacsclient -nw";
    grep = "${pkgs.ripgrep}/bin/rg";
  };
  
}
