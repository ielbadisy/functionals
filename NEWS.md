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
