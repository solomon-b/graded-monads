final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
      (hfinal: hprev: {
        graded-monads = hfinal.callCabal2nix "graded-monads" ./. { };
      });
  });
}
