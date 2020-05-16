with (import ./default.nix);

hsPkgs.shellFor {
  packages = ps: with ps; [
    fermatslastmargin
  ];

  withHoogle = false;

  buildInputs = with hsPkgs; with pkgs; [
    hlint
    stylish-haskell
    ghcid
    cabal-install
    poppler_utils
  ];

  exactDeps = true;
}
