{ }:
with rec {
  sources = import ./sources.nix;
  pkgs_ = import sources.nixpkgs {};
  pkgs = import sources.nixpkgs {
    overlays = [
      (self: super: super.lib.recursiveUpdate super {
        haskell = super.lib.recursiveUpdate super.haskell {
          packages = super.lib.recursiveUpdate super.haskell.packages {
            ghc884 = super.haskell.packages.ghc884.override (old: {
              overrides = super.lib.composeExtensions
                (old.overrides or (_: _: {}))
                (hself: hsuper: {
                  fermatslastmargin = hself.callCabal2nix "fermatslastmargin" (super.lib.cleanSource ../.) {};
                });
            });
          };
        };
      })
    ];
  };

};

rec {
  inherit pkgs;
  inherit sources;
  hsPkgs = pkgs.haskell.packages.ghc884;
}
