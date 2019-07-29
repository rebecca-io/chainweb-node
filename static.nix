{ rpRef ? "21242967fbfae75c1cc4907449b0730dc0a0d906"
, rpSha ? "0sxygr1zdsraybnr4m71m36mb95qsy5vczxxm330h0qg2ijsz1nm"
, system ? builtins.currentSystem
, runTests ? true
}:

let

rpSrc = /Users/doug/code/public/reflex-platform;
# rpSrc = builtins.fetchTarball {
#   url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
#   sha256 = rpSha;
# };
overlay = self: super: {
  z3 = super.z3.overrideAttrs (drv: {
    name = "z3-4.8.5";
    version = "4.8.5";
    patches = [];
    src = self.fetchFromGitHub {
      owner = "Z3Prover";
      repo = "z3";
      rev = "e79542cc689d52ec4cb34ce4ae3fbe56e7a0bf70";
      sha256 = "11sy98clv7ln0a5vqxzvh6wwqbswsjbik2084hav5kfws4xvklfa";
    };
  });
};
rp = import rpSrc { inherit system; nixpkgsOverlays = [ overlay ]; };

proj = rp.ghcMusl64.override {
  overrides = self: super: (import ./overrides.nix rp.nixpkgs self super) // {
  };
};

in proj.chainweb
