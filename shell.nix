{ pkgs ? import ./pin.nix }:
let 
    # this shell provides us a fast build setup
    # default.nix is generated by hpack & cabal2nix
    origBuild = import ./default.nix { };
    wTools = pkgs.haskell.lib.overrideCabal origBuild (drv: {
      libraryToolDepends = drv.libraryToolDepends ++ [
        pkgs.ghcid 
        pkgs.cabal-install
        pkgs.haskellPackages.hasktags
        ];
    });
in 
wTools.env
