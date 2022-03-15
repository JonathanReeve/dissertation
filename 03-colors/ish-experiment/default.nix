let
  # pkgs = import <nixpkgs> { };
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  pkgs = import (builtins.fetchTarball {
    name = "pinnedNixpkgs";
    url = "https://github.com/nixos/nixpkgs/archive/0eb0ddc4dbe3cd5415c6b6e657538eb809fc3778.tar.gz";
    sha256 = "09ammqxzqydj97lk5iwrlg4xnj7b2pnhj6qpxa0pbp9z0651yvz6";
    }) {};
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
          (all-hies.selection { selector = p: { inherit (p) ghc882; }; })
        ]);
    source-overrides = {
      plotlyhs = builtins.fetchTarball "https://github.com/JonathanReeve/plotlyhs/archive/0fcf833.tar.gz";
    };
  }

