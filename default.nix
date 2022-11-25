{nixpkgs ? import (builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2022-07-31";
  # Commit hash for nixos-unstable as of 2018-09-12
  url = "https://github.com/nixos/nixpkgs/archive/7b9be38c7250b22d829ab6effdee90d5e40c6e5c.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1jgcsgzvxnc27wby4n7jyp5w67461qm5x519xkpz2h14zbjjndm7";
}) {}
}:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          haskell-language-server
          lucid clay shake regex-compat text-regex-replace with-utf8
          pandoc
          pandoc-crossref
          tagsoup
          warp wai-app-static # Dev webserver
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  propagatedBuildInputs = with pkgs; [ graphviz ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
