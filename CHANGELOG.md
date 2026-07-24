# Revision history for graded-monads

## Upcoming
* Adds a `Weaken` preorder on grades with membership witnesses and `Subset` injection, making return grade-polymorphic and allowing error grades to be weakened.
* Grades errors by canonical type-level sets via the new `sorted-type-sets` package: the set-algebra families (`Union`, `Nub`, `Remove`, `Delete`) deduplicate error grades, while writer grades keep their list semantics.
* Adds a narrowing `gcatch` that handles a single error and removes it from the grade.
* Adds a `graded-monads:laws` sublibrary with the weakening reflexivity law.
* Expands GHC support through 9.12; bumps nixpkgs to 26.05 and modernizes the CI tooling.
* Bumps the pinned `monoidal-functors` dependency and widens the `mtl` and `these` version bounds.
* Modernizes the Nix flake: extracts an `overlay.nix`, switches to `eachDefaultSystem`, and exposes a `formatter` and `overlays.default`.
* Replaces the ad-hoc dev workflow with a `justfile` (format, lint, build, test, haddock, and release commands).
