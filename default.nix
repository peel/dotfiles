{ pkgs ? import <nixpkgs> {}
, machine ? "fff666"
, repoUrl ? "https://github.com/peel/dotfiles.git"
, nurpkgs ? "https://github.com/peel/nur-packages.git"
, channel ? "nixpkgs-unstable"
, targetDir ? "$HOME/wrk"
}:

let
  darwin = ''
    echo >&2
    echo >&2 "Building initial configuration..."
    echo >&2
    source /etc/static/bashrc
    darwin-rebuild switch -I "darwin-config=$HOME/.config/nixpkgs/machines/darwin/configuration.nix" -I "nixpkgs-overlays=$HOME/.config/nixpkgs/overlays" -I "nurpkgs-peel=$HOME/.config/nurpkgs" -I "setup=$HOME/.config/nixpkgs/setup"
  '';
  install = pkgs.writeScript "install" ''
    set -e

    echo >&2
    echo >&2 "Installing..."
    echo >&2

    ${pkgs.lib.optionalString pkgs.stdenvNoCC.isDarwin ''
    if [ ! command -v darwin-rebuild >/dev/null 2>&1 ]; then
        mkdir -p ./nix-darwin && cd ./nix-darwin
        nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
        yes | ./result/bin/darwin-installer
        cd .. && rm -rf ./nix-darwin
    fi
    ''}

    if [ ! -d ${targetDir}/nurpkgs ]; then
        echo "Setting up nurpkgs repository" >&2
        mkdir -p ${targetDir}
        git clone ${nurpkgs} ${targetDir}/nurpkgs
    fi

    if [ ! -d ${targetDir}/dotfiles ]; then
        echo "Setting up dotfiles repository" >&2
        mkdir -p ${targetDir}/dotfiles
        git clone ${repoUrl} ${targetDir}/dotfiles
    fi

    ${link} "$@"
    ${pkgs.lib.optionalString pkgs.stdenvNoCC.isDarwin darwin}
  '';
  link = pkgs.writeScript "link" ''
    set -e

    echo >&2
    echo >&2 "Linking..."
    echo >&2

    echo "$@"

    mkdir -p ~/.config
    ln -fs ${targetDir}/nurpkgs ~/.config/nurpkgs
    ln -fs ${targetDir}/dotfiles ~/.config/nixpkgs
    ${pkgs.lib.optionalString pkgs.stdenvNoCC.isLinux ''
    mv /etc/nixos /etc/nixos.bak || true
    ln -fs ${targetDir}/dotfiles/machines/$1 /etc/nixos
    ''}
  '';
  unlink = pkgs.writeScript "unlink" ''
    set -e

    echo >&2
    echo >&2 "Unlinking..."
    echo >&2
    rm ~/.config/nixpkgs
    rm /etc/nixos
    mv /etc/nixos.bak /etc/nixos
  '';
  uninstall = pkgs.writeScript "uninstall" ''
    ${unlink}

    echo >&2
    echo >&2 "Cleaning up..."
    echo >&2

    rm -rf ~/.config/nurpkgs
  '';
  switch = pkgs.writeScript "switch" ''
    set -e

    cd ${targetDir}/dotfiles

    echo >&2
    echo >&2 "Tagging working config..."
    echo >&2
   
    git branch -f update HEAD

    echo >&2
    echo >&2 "Switching environment..."
    echo >&2
   
    ${pkgs.lib.optionalString pkgs.stdenvNoCC.isDarwin ''
      darwin-rebuild switch -j 4
      echo "Current generation: $(darwin-rebuild --list-generations | tail -1)"
    ''}
    ${pkgs.lib.optionalString pkgs.stdenvNoCC.isLinux ''
      nixos-rebuild switch
    ''}

    echo >&2
    echo >&2 "Tagging updated..."
    echo >&2

    git branch -f working update
    git branch -D update
    git push
  '';
in pkgs.stdenvNoCC.mkDerivation {
  name = "dotfiles";
  preferLocalBuild = true;
  propagatedBuildInputs = [ pkgs.git ];
  propagatedUserEnvPkgs = [ pkgs.git ];
  
  unpackPhase = ":";

  installPhase = ''
    mkdir -p $out/bin
    echo "$shellHook" > $out/bin/dotfiles
    chmod +x $out/bin/dotfiles
  '';

  shellHook = ''
    set -e

    while [ "$#" -gt 0 ]; do
        i="$1"; shift
        case "$i" in
            install)
                ${install} "$@"
                exit
                ;;
            link)
                ${link} "$@"
                exit
                ;;
            switch)
                ${switch}
                exit
                ;;
            unlink)
                ${unlink}
                exit
                ;;
            uninstall)
                ${uninstall}
                exit
                ;;
           *)
                echo "dotfiles: [help] [install machine-name] [uninstall] [link machine-name] [unlink] [switch]"
                exit
                ;;
        esac
    done
    exit
  '';

  passthru.check = pkgs.stdenvNoCC.mkDerivation {
     name = "run-dotfiles-test";
     shellHook = ''
        set -e
        echo >&2 "running dotfiles tests..."
        echo >&2
        echo >&2 "checking repository"
        test -d ${targetDir}
        exit
    '';
  };
}
