let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ]);
    source-overrides = {
      # numhask-space = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.numhask-space;
      plotlyhs = builtins.fetchTarball "https://github.com/JonathanReeve/plotlyhs/archive/d07770b.tar.gz";
        # pkgs.haskell.lib.callCabal2Nix 
        };
  }
