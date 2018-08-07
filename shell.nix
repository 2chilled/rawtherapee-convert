{pkgs ? import <nixpkgs> {}}:

let haskellPkgs = pkgs.haskellPackages;
in {
  scrapeChangesStackEnv = pkgs.haskell.lib.buildStackProject {
    name = "rawtherapeeProcStackEnv";
    buildInputs = with haskellPkgs; [
      stack pkgs.zlib hasktags hdevtools ghcid #ghc-mod
    ];
    ghc = pkgs.haskell.packages.ghc802.ghc;
  };
}
