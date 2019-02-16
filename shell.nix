let pkgs =
  let localPkgs = import <nixpkgs> {};
  in import (localPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "91c7157919081c5bbc8cb808e27d89ed4a8cbef7";
    sha256 = "0hqscxpkgnpvm8354sxf3vi7j6chzv0dsy3g8pgmhfacha4hfwl5";
  }) {};
  myHaskTags = pkgs.haskell.lib.dontCheck haskellPkgs.hasktags;
  haskellPkgs = pkgs.haskell.packages.ghc863;
  hiePkgs = import (pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "6794005f909600679d0b7894d0e7140985920775";
    sha256 = "0pc90ns0xcsa6b630d8kkq5zg8yzszbgd7qmnylkqpa0l58zvnpn";
  }){};
in {
  scrapeChangesStackEnv = pkgs.haskell.lib.buildStackProject {
    name = "rawtherapeeProcStackEnv";
    buildInputs = with haskellPkgs; [
      hiePkgs.hie86
      stack
      pkgs.zlib
      myHaskTags
      hoogle
      #brittany
      /*hdevtools*/ /*ghcid*/ #ghc-mod
      pkgs.fzf
    ];
    ghc = haskellPkgs.ghc;
    #HIE_HOOGLE_DATABASE=./.stack-work/hoogle/x86_64-linux-nix/lts-12.26/8.4.4/database.hoo;
  };
}
