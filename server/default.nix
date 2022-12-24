{ pkgs ? import <nixpkgs> { }, ... }:
let 
  resource-pool = pkgs.haskellPackages.resource-pool_0_3_1_0;
in

pkgs.haskellPackages.callCabal2nix "lista" ./. { inherit resource-pool; }
