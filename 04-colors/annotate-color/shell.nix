{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    regex-compat lucid replace-attoparsec hspec cabal-install text-regex-replace optparse-generic
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
