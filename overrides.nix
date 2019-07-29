pkgs: runTests: self: super: with pkgs.haskell.lib;
let # Working on getting this function upstreamed into nixpkgs, but
    # this actually gets things directly from hackage and doesn't
    # depend on the state of nixpkgs.  Should allow us to have fewer
    # github overrides.
    callHackageDirect = {pkg, ver, sha256}@args:
      let pkgver = "${pkg}-${ver}";
      in self.callCabal2nix pkg (pkgs.fetchzip {
           url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
           inherit sha256;
         }) {};

    pactSrc = pkgs.fetchFromGitHub {
      owner = "kadena-io";
      repo = "pact";
      rev = "dedc8c08ff3e9fe99c4618279435a83dc02f3504";
      sha256 = "0rjh9h4zmlwasc77pwxxg79bgxya5pls6syrmjgn4b65qhb47wld";
      };
in

(import "${pactSrc}/overrides.nix" pkgs self super) // {
  aeson = callHackageDirect {
    pkg = "aeson";
    ver = "1.4.3.0";
    sha256 = "13lim8vv78m9lhn7qfjswg7ax825gn0v75gcb80hckxawgk8zxc1";
  };

  chainweb = justStaticExecutables (enableDWARFDebugging (overrideCabal super.chainweb (drv: {
    doCheck = runTests;
    doHaddock = runTests;
    testTarget = "--test-option=--hide-successes";
  })));

  chainweb-storage = pkgs.haskell.lib.dontCheck (self.callCabal2nix "chainweb-storage" (pkgs.fetchFromGitHub {
    owner = "kadena-io";
    repo = "chainweb-storage";
    rev = "4a345323cd50f1fd24ed9565c0deeea5cc376db6";
    sha256 = "0alqvb3hx7bvhq1mcpq8m0l2jcwz4b4xdp1d20r2fflbraqrvmgs";
  }) {});

  configuration-tools = dontCheck (callHackageDirect {
    pkg = "configuration-tools";
    ver = "0.4.1";
    sha256 = "1sbn4dbb2y1gwdwjvz5vf6a1g349z0jha5iz4dmp2v67dv86fzs5";
  });

  digraph = dontCheck (callHackageDirect {
      pkg = "digraph";
      ver = "0.1.0.2";
      sha256 = "1alqdzzlw8ns6hy8vh3ic4ign7jjxxa0cyxkv26zz7k2dihf3hzg";
  });

  fake = doJailbreak (callHackageDirect {
    pkg = "fake";
    ver = "0.1.1.2";
    sha256 = "1swp4j80761rfb0xiwshf0zal02ykwrbv49iyjay9ivvka367wk9";
  });

  massiv = callHackageDirect {
    pkg = "massiv";
    ver = "0.3.6.0";
    sha256 = "1wcvs705b377zm0l33mhpzy1kyhwclxqkd5gzhk7dsd6ymnrlmpm";
  };

  merkle-log = callHackageDirect {
      pkg = "merkle-log";
      ver = "0.1.0.0";
      sha256 = "10jk274sbvsrr7varxa72jvh54n22qpw7d4p2wy7415bmij3y81p";
  };

  pact = dontCheck ( addBuildDepend (self.callCabal2nix "pact" pactSrc {}) pkgs.z3);
  rocksdb-haskell = dontCheck super.rocksdb-haskell;

  scheduler = dontCheck (callHackageDirect {
    pkg = "scheduler";
    ver = "1.4.1";
    sha256 = "00nr6bdazbaqjv2fw55krbi7g8xi2vdvhdvb6z83ag905c79jyci";
  });

  # Our own custom fork
  thyme = dontCheck (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
    owner = "kadena-io";
    repo = "thyme";
    rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
    sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
  }) {});

  tls = callHackageDirect {
      pkg = "tls";
      ver = "1.5.0";
      sha256 = "05srd9lssgs437h45mawaxbd79lqyxj7qx7fx2sdgncr6m0h8vm0";
  };

  warp-tls = callHackageDirect {
      pkg = "warp-tls";
      ver = "3.2.7";
      sha256 = "1xaqk1qrcxh5lv92v1hvdsim7v8plrp0b3wyzkhzq9xqhmk24fvj";
  };

}