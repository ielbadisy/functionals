## Summary

This is a feature release (0.6.0). The version currently on CRAN is 0.5.0;
0.5.1 was prepared but never submitted, so its CRAN-check cleanups (a stray
`LazyData` field, overfull PDF-manual lines, DESCRIPTION metadata) are rolled
into this release and listed in NEWS.md under 0.5.1.

New in 0.6.0:

* `.on_error` argument (`"stop"` / `"pass"` / `"fill"`) for a uniform error
  policy across sequential and parallel execution.
* `.seed` argument giving each task an independent L'Ecuyer-CMRG RNG stream, so
  parallel Monte Carlo runs are reproducible; the caller's global RNG state is
  saved and restored.
* `floop()` is deprecated in favour of `fmap()` / `fwalk()` (warns, still works).
* Two PDF vignettes and a reproducible benchmark script under `inst/`.

## Test environments

* Local Linux, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 1 note

* "New maintainer" / "unable to verify current time" may appear as in prior
  submissions (maintainer family-name casing "EL BADISY" -> "El Badisy"; local
  clock-check). Same person, same email address.

## Reverse dependencies

None on CRAN.
