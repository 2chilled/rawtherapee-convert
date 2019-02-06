let pkgs =
  let localPkgs = import <nixpkgs> {};
  in import (localPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "cfbf9f0664050a31599e3f348a6b9ec8b13ca87b";
    sha256 = "08k3hzgl43frz7bm0hhx5gyhrvn39yn2k76x9wj0wsddh76wpijh";
  }) {};
  myHaskTags = pkgs.haskell.lib.dontCheck haskellPkgs.hasktags;
  haskellPkgs = pkgs.haskell.packages.ghc844;
  hiePkgs = import (pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "19f47e0bf2e2f1a793bf87d64bf8266062f422b1";
    sha256 = "1px146agwmsi0nznc1zd9zmhgjczz6zlb5yf21sp4mixzzbjsasq";
  }){};
in {
  scrapeChangesStackEnv = pkgs.haskell.lib.buildStackProject {
    name = "rawtherapeeProcStackEnv";
    buildInputs = with haskellPkgs; [
      hiePkgs.hie84
      stack
      pkgs.zlib
      myHaskTags
      hoogle
      brittany
      /*hdevtools*/ /*ghcid*/ #ghc-mod
      pkgs.fzf
    ];
    ghc = haskellPkgs.ghc;
    #HIE_HOOGLE_DATABASE=./.stack-work/hoogle/x86_64-linux-nix/lts-12.26/8.4.4/database.hoo;
  };
}
