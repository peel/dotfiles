{ config, pkgs, ...}:

{
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
      __prompt_nix() {
        [ -z "$IN_NIX_SHELL" ] && echo "λ" || echo ""
      }
      PS1='\W$(__git_ps1 " - %s") $(__prompt_nix) '
    '';
  };

  environment.shellAliases = {
    k9 = "kill -9";
    dc = "docker-compose";
    less = "less -R";
    tailf = "tail -f";
    ls = "ls -Gh";
    ll = "ls -al";
    df = "df -h";
    du = "du -h -d 2";
    grep = "${pkgs.ripgrep}/bin/rg";
    n = "nix";
    ns = "nix-shell";
  };
  
}
