{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, Agda, base, bytestring
      , composition-extra, lib, postgresql-simple, QuickCheck
      , quickcheck-instances, resource-pool, servant-docs, servant-server
      , uuid, uuid-types, warp
      }:
      mkDerivation {
        pname = "lista";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson Agda base bytestring composition-extra postgresql-simple
          QuickCheck quickcheck-instances resource-pool servant-docs
          servant-server uuid uuid-types warp
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
