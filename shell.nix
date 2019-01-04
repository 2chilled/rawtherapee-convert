let pkgs =
  let localPkgs = import <nixpkgs> {};
  in import (localPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "c89cbdcd48bf11fbb9fb9ebaeaa59aad566a8648";
    sha256 = "04ykb9khza259fqv80a0maw66m6wgw1cpnmmhhnm1dgfrylfskr2";
  }) {};
  myHaskTags = pkgs.haskell.lib.dontCheck haskellPkgs.hasktags;
  haskellPkgs = pkgs.haskell.packages.ghc844;
  hiePkgs = import (pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "a7ef4c4ceef1dbf46aabff68a4b9bd86d41d0886";
    sha256 = "1hx449l001jc6ijn9zxx30zr1xr2nnrv7qmhpsqwj8wp6s4zyxw8";
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
      /*hdevtools*/ /*ghcid*/ #ghc-mod
      pkgs.fzf
    ];
    ghc = haskellPkgs.ghc;
    HIE_HOOGLE_DATABASE=./.stack-work/hoogle/x86_64-linux-nix/lts-12.26/8.4.4/database.hoo;
  };
}
