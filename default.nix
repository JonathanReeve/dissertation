{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:
let
  inherit (nixpkgs) pkgs;
  # TODO
  # pandoc-sidenote = 
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          haskell-language-server
          lucid clay shake regex-compat text-regex-replace with-utf8
          pandoc pandoc-citeproc pandoc-crossref #pandoc-sidenote
          warp wai-app-static # Dev webserver
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  propagatedBuildInputs = with pkgs; [ graphviz ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
