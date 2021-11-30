with (import ./default.nix);

hsPkgs.shellFor {
  packages = ps: with ps; [ fermatslastmargin ];

  withHoogle = false;

  buildInputs = with hsPkgs;
    with pkgs; [
      cabal-install
      ghcid
      haskell-language-server
      hlint
      poppler_utils
      stylish-haskell
    ];

  exactDeps = true;
}
