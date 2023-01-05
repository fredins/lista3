let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  withHoogle = true;
  modifier = drv:
  pkgs.haskell.lib.addBuildTools drv (
    (with pkgs; [ ghc hlint haskell-language-server ])
    ++
    (with pkgs.haskellPackages; [ cabal-install stylish-haskell ])
  );
}

