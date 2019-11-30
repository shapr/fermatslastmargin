with (import ./nix/default.nix {});

rec {
  inherit pkgs;
  inherit hsPkgs;

  fermatslastmargin = hsPkgs.fermatslastmargin.overrideAttrs (old: rec {
    buildInputs = (old.buildInputs or []) ++ [ pkgs.makeWrapper ];

    # get pdftocairo in PATH
    postInstall = ''
      wrapProgram $out/bin/fermatslastmargin --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.poppler_utils ]}
    '';
  });
}
