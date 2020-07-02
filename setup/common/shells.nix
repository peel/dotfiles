{ config, pkgs, ...}:

{
  environment.shells = [ pkgs.bashInteractive  ];
  environment.systemPackages = with pkgs; [
    coreutils
    ripgrep
  ];
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
    d = "docker";
    dc = "docker-compose";
    less = "less -R";
    ls = "ls -Gh";
    ll = "ls -al";
    df = "df -h";
    ns = "nix-shell";
  };
  
}
