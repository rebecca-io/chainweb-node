{ rpRef ? "21242967fbfae75c1cc4907449b0730dc0a0d906"
, rpSha ? "0sxygr1zdsraybnr4m71m36mb95qsy5vczxxm330h0qg2ijsz1nm"
, system ? builtins.currentSystem
, runTests ? false
, profileChainweb ? false
}:

let

rpSrc = builtins.fetchTarball {
  url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
  sha256 = rpSha;
};
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
rp = import rpSrc {
  inherit system;
  nixpkgsOverlays = [ overlay ];
  enableLibraryProfiling = true;
};

proj = rp.project ({ pkgs, ... }:
  let gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
        sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
      }) {};
  in
  {
    name = "chainweb";
    overrides = import ./overrides.nix pkgs runTests profileChainweb;
    packages = {
      chainweb = gitignore.gitignoreSource
        [".git" ".gitlab-ci.yml" "CHANGELOG.md" "README.md" "future-work.md"] ./.;
    };
    shellToolOverrides = ghc: super: {
      stack = pkgs.stack;
      cabal-install = pkgs.haskellPackages.cabal-install;
      ghcid = pkgs.haskellPackages.ghcid;
      z3 = pkgs.z3;
    };
    shells = {
      ghc = ["chainweb"];
    };

  });

in proj.ghc.chainweb
