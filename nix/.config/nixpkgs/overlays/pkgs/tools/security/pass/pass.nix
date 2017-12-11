{  pkgs, stdenv, makeWrapper, qrencode, gnupg, getopt }:
let

  src = pkgs.fetchGit {
    url = "https://git.zx2c4.com/password-store.git"
    rev = "1.7.1";
    sha = "";
  };
  # pass-extension-tail = pkgs.fetchFromGitHub {
  #   owner = "peel";
  #   repo = "pass-extension-tail";
  #   rev = "";
  #   sha = "";
  # };
  # pass-tomb = pkgs.fetchFromGitHub {
  #   owner = "peel";
  #   repo = "pass-tomb";
  #   rev = "";
  #   sha = "";
  # };
  # pass-otp = pkgs.fetchFromGitHub {
  #   owner = "peel";
  #   repo = "pass-otp";
  #   rev = "";
  #   sha = "";
  # };
  # pass-update = pkgs.fetchFromGitHub {
  #   owner = "peel";
  #   repo = "pass-update";
  #   rev = "";
  #   sha = "";
  # };
in
stdenv.mkDerivation {
  name = "Wrapper";
  buildInputs = [ makeWrapper qrencode tree gnupg getopt ];
  phases = [ "buildPhase" ];
  buildCommand = ''
    mkdir -p $out/bin
    makeWrapper "${pkgs.password-store}/bin/pass" $out/bin/pass
      --set PASSWORD_STORE_CLIP_TIME 45
      --set PASSWORD_STORE_UMASK 077
      --set PASSWORD_STORE_ENABLE_EXTENSIONS true
      --set PASSWORD_STORE_EXTENSIONS_DIR $out/extensions
  '';
}
