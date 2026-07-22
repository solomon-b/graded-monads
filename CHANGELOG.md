# Revision history for graded-monads

## Upcoming
* Expands GHC support through 9.12; bumps nixpkgs to 26.05 and modernizes the CI tooling.
* Bumps the pinned `monoidal-functors` dependency and widens the `mtl` and `these` version bounds.
* Modernizes the Nix flake: extracts an `overlay.nix`, switches to `eachDefaultSystem`, and exposes a `formatter` and `overlays.default`.
* Replaces the ad-hoc dev workflow with a `justfile` (format, lint, build, test, haddock, and release commands).
