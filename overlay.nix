final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
      (hfinal: hprev: {
        sorted-type-sets = hfinal.callCabal2nix "sorted-type-sets" ./sorted-type-sets { };
        graded-monads = hfinal.callCabal2nix "graded-monads" ./. { };
      });
  });
}
