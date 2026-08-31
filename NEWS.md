# functionals 0.6.0

## New features

- `fapply()` and every mapper (`fmap()`, `fmapn()`, `fmapr()`, `fmapc()`,
  `fmapg()`, `fwalk()`, `fcv()`, `frepeat()`) gain `.on_error`: `"stop"`
  (default, unchanged behaviour), `"pass"` (continue and store a
  `functionals_error` object in the failed position, then emit a one-line
  summary warning), or `"fill"` (substitute `.fill`). The policy behaves
  identically in sequential, fork, and socket-cluster execution.
- New `.seed` argument for reproducible iteration. Each task runs with its own
  L'Ecuyer-CMRG random-number stream, so results do not depend on `ncores` or
  task-completion order. The caller's global RNG state is saved and restored.
  `frepeat(..., .seed =)` makes parallel Monte Carlo runs reproducible.
- Two PDF vignettes: "Functional programming in R with functionals" (includes a
  single-threaded benchmark against purrr) and "Functionals in the modeling
  workflow" (cross-validation, bootstrap, grouped fits, learner benchmarks).
- Reproducible benchmark script shipped at
  `system.file("benchmarks/bench-purrr.R", package = "functionals")`.

## Deprecations

- `floop()` is deprecated. It never did anything beyond `fmap()` (collect
  results) and `fwalk()` (side effects only); it now warns and forwards to
  them, and will be removed in a future release.

## Fixes and internals

- `ncores` handling is unified through a single internal normaliser: `NULL`,
  `1`, and `1L` all mean sequential; invalid values warn and fall back to
  sequential, consistently across every function.
- `fmapn()` now forwards `...` to `.f` instead of leaking the extra arguments
  into an internal helper (previously an error whenever `...` was non-empty).

# functionals 0.5.1

- Removed the stray `LazyData: true` field (the package ships no `data/`
  directory, so it triggered an "Omitted 'LazyData'" note at build time).
- Wrapped long example lines in `floop()`, `fmapc()`, `fmapg()`, `fmapn()`,
  and `frepeat()` that were overflowing the PDF manual's page width and
  triggering a LaTeX WARNING under `R CMD check --as-cran`.
- Simplified `DESCRIPTION`'s `Description:` field: dropped a stray
  cross-reference to another package by name and a GitHub link (URL/
  BugReports already cover that).
- Standardized `Authors@R` family-name casing and split `URL`/`BugReports`
  so `URL` holds only the CRAN package page and `BugReports` only the
  GitHub issues page, matching the pattern used across all packages.

- Fixed `fapply()` and `fmap()` progress reporting so sequential, multicore, and
  cluster-backed execution advance on completed tasks rather than internal
  chunk boundaries.
- Improved progress-bar output stability when user code emits messages during
  tracked execution.
- Added progress-system regression tests covering sequential timing, exact
  parallel counts, and error propagation in cluster and multicore execution.
- Updated package documentation to describe completion-driven progress
  semantics.
