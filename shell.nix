{pkgs ? import <nixpkgs> {}}:

let haskellPkgs = pkgs.haskellPackages;
in {
  scrapeChangesStackEnv = pkgs.haskell.lib.buildStackProject {
    name = "rawtherapeeProcStackEnv";
    buildInputs = with haskellPkgs; [
      stack pkgs.zlib hasktags hdevtools ghc-mod
    ];
    ghc = pkgs.haskell.packages.ghc7103.ghc;
  };
}
