self: super: {

installApplication = 
  { name, appname ? name, version, src, description, homepage, 
    postInstall ? "", sourceRoot ? ".", ... }:
  with super; stdenv.mkDerivation {
    name = "${name}-${version}";
    version = "${version}";
    src = src;
    buildInputs = [ undmg unzip ];
    sourceRoot = sourceRoot;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p "$out/Applications/${appname}.app"
      cp -pR * "$out/Applications/${appname}.app"
    '' + postInstall;
    meta = with stdenv.lib; {
      description = description;
      homepage = homepage;
      platforms = platforms.darwin;
    };
  };

Alfred = self.installApplication rec {
  name = "Alfred";
  version = "3.8_959";
  sourceRoot = "Alfred 3.app";
  src = super.fetchurl {
    url = "https://cachefly.alfredapp.com/Alfred_${version}.dmg";
    sha256 = "1md43m33fxd5c4gz4va1j7b62amhp7zzh1za7sjj4g4iccq5zl8x";
    # date = 2018-09-28T23:36:13-0700;
  };
  description = "Alfred is an award-winning app for macOS which boosts your efficiency with hotkeys, keywords, text expansion and more. Search your Mac and the web, and be more productive with custom actions to control your Mac.";
  homepage = https://alfredapp.com;
};

Calibre = self.installApplication rec {
  name = "Calibre";
  version = "3.40.1";
  sourceRoot = "Calibre.app";
  src = super.fetchurl {
    url = "https://download.calibre-ebook.com/${version}/calibre-${version}.dmg";
    sha256 = "1afcc552c1a53a65423da64bc59cff4fe5f27dc51dce67bd15b63a3851e6f121";
    # date = 2018-03-10T23:36:13-0700;
  };
  description = "Calibre is a one stop solution for all your ebook needs.";
  homepage = https://calibre-ebook.com;
  # appcast = https://github.com/kovidgoyal/calibre/releases.atom;
};

Dash = self.installApplication rec {
  name = "Dash";
  version = "4.5.1";
  sourceRoot = "Dash.app";
  src = super.fetchurl {
    url = https://kapeli.com/downloads/v4/Dash.zip;
    sha256 = "1dkrzh4l5an0z7qv25irmagssaymscjzzgdn1jha11sqhhna9lh1";
    # date = 2018-09-28T23:36:13-0700;
  };
  description = "Dash is an API Documentation Browser and Code Snippet Manager";
  homepage = https://kapeli.com/dash;
};

Docker = self.installApplication rec {
  name = "Docker";
  version = "2.0.0.3";
  versionId = "31259";
  sourceRoot = "Docker.app";
  src = super.fetchurl {
    url = "https://download.docker.com/mac/stable/${versionId}/Docker.dmg";
    sha256 = "09gwqdppnzw7hhlmgxakczxql4jfknk4ayc5z09g4kr8agqn4m55";
    # date = 2018-09-28T11:14:33-0700;
  };
  description = ''
    Docker CE for Mac is an easy-to-install desktop app for building,
    debugging, and testing Dockerized apps on a Mac
  '';
  homepage = https://store.docker.com/editions/community/docker-ce-desktop-mac;
};
}
