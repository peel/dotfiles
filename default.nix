{ pkgs ? import <nixpkgs> {}
, repoUrl ? "https://github.com/peel/dotfiles.git"
, nurpkgs ? "https://github.com/peel/nur-packages.git"
, channel ? "nixpkgs-unstable"
, targetDir ? "$HOME/wrk/dotfiles"
}:

let
  darwin = ''
    echo >&2
    echo >&2 "Building initial configuration..."
    echo >&2
    darwin-rebuild switch -j4 -I "darwin-config=$HOME/.config/nixpkgs/machines/darwin/configuration.nix" -I "nixpkgs-overlays=$HOME/.config/nixpkgs/overlays" -I "nurpkgs-peel=$HOME/.config/nurpkgs" -I "setup=$HOME/.config/nixpkgs/setup"
  '';
  install = pkgs.writeScript "install" ''
    set -e

    echo >&2
    echo >&2 "Installing..."
    echo >&2

    nix-channel --add https://nixos.org/channels/${channel} nixpkgs
    nix-channel --update nixpkgs

    ${pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
    if ! command -v darwin-rebuild >/dev/null 2>&1; then
        mkdir -p ./nix-darwin && cd ./nix-darwin
        nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
        yes | ./result/bin/darwin-installer -j4
        cd .. && rm -rf ./nix-darwin
    fi
    ''}

    if [ ! -d $HOME/.config/nurpkgs ]; then
        echo "setting up nurpkgs repository" >&2
        mkdir -p ${targetDir}
        git clone --depth=1 ${nurpkgs} $HOME/.config/nurpkgs
    fi

    if [ ! -d ${targetDir} ]; then
        echo "setting up dotfiles repository" >&2
        mkdir -p ${targetDir}
        git clone --depth=1 ${repoUrl} ${targetDir}
    fi

    ${link}
    ${pkgs.lib.optionalString pkgs.stdenv.isDarwin darwin}
  '';
  link = pkgs.writeScript "link" ''
    set -e

    echo >&2
    echo >&2 "Linking..."
    echo >&2

    mkdir -p ~/.config

    for f in ${targetDir}/*; do
      if [ -d $f  ]; then
        echo "Relinking $f"
        cd ${targetDir}
        ${pkgs.stow}/bin/stow -t ~ -R $(basename $f)
      fi
    done
  '';
  unlink = pkgs.writeScript "unlink" ''
    set -e

    echo >&2
    echo >&2 "Unlinking..."
    echo >&2

    for f in ${targetDir}/*; do
      echo "Unlinking $f"
      cd ${targetDir}
      ${pkgs.stow}/bin/stow -t ~ -D $(basename $f)
    done
  '';
  uninstall = pkgs.writeScript "uninstall" ''
    ${unlink}

    echo >&2
    echo >&2 "Cleaning up..."
    echo >&2

    if [ -d ${targetDir} ]; then
        echo "removing dotfiles repository" >&2
        rm -rf ${targetDir}
    fi
  '';
  switch = pkgs.writeScript "switch" ''
    set -e

    echo >&2
    echo >&2 "Tagging working config..."
    echo >&2
   
    git branch -f update HEAD

    echo >&2
    echo >&2 "Switching environment..."
    echo >&2
   
    ${pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
      darwin-rebuild switch -j 4
      echo "Current generation: $(darwin-rebuild --list-generations | tail -1)"
    ''}
    ${pkgs.lib.optionalString pkgs.stdenv.isLinux ''
      nixos-rebuild switch
    ''}

    ${link}

    echo >&2
    echo >&2 "Tagging updated..."
    echo >&2

    git branch -f working update
    git branch -D update
    git push
  '';
  update = pkgs.writeScript "update" ''
    set -e

    echo >&2
    echo >&2 "Updating channels..."
    echo >&2

    nix-channel --update
    ${pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
      nix-channel --update darwin
    ''}

    ${switch}
  '';
in pkgs.stdenv.mkDerivation {
  name = "dotfiles";
  preferLocalBuild = true;

  unpackPhase = ":";

  installPhase = ''
    mkdir -p $out/bin
    echo "$shellHook" > $out/bin/dotfiles
    chmod +x $out/bin/dotfiles
  '';

  shellHook = ''
    set -e

    while [ "$#" -gt 0 ]; do
        i="$1"; shift 1
        case "$i" in
           help)
                echo "dotfiles: [help] [install] [uninstall] [link] [unlink] [switch] [update]"
                exit
                ;;
            link)
                ${link}
                ;;
            switch)
                ${switch}
                ;;
            unlink)
                ${unlink}
                ;;
            uninstall)
                ${uninstall}
                ;;
            update)
                ${update}
                ;;
            *)
                ${install}
                ;;
        esac
    done
    exit
  '';

  passthru.check = pkgs.stdenv.mkDerivation {
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
