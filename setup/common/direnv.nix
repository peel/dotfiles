{config, pkgs, ...}:

let
  hook = shell: "eval \"$(${pkgs.direnv}/bin/direnv hook ${shell})\"";
in {
  environment.systemPackages = [ pkgs.direnv ];
  programs.bash.interactiveShellInit =  hook "bash";
}
