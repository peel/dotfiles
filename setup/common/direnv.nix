{config, pkgs, ...}:

let
  hook = shell: "eval \"$(${pkgs.direnv}/bin/direnv hook ${shell})\"";
in {
  environment.systemPackages = [ pkgs.direnv ];
  environment.shellAliases.d = hook "bash";
  programs.bash.interactiveShellInit =  hook "bash";
}
