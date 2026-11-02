## Update submission (0.5.1 -> 0.6.0)

Feature release; see NEWS.md for details.

* `.on_error` argument (`"stop"` / `"pass"` / `"fill"`) for a uniform error
  policy across sequential and parallel execution. The default (`"stop"`)
  keeps the previous behaviour.
* `.seed` argument giving each task an independent L'Ecuyer-CMRG RNG stream,
  so parallel runs are reproducible; the caller's global RNG state is saved
  and restored.
* `floop()` is deprecated in favour of `fmap()` / `fwalk()` (it warns and
  still works).
* `fmapn()` now forwards `...` to `.f` (previously an error).
* Two PDF vignettes and a reproducible benchmark script under `inst/`.

## Test environments

* Local: Ubuntu 24.04, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Five packages on CRAN import functionals: funcml, mcstatsim, mimar,
phynotype and survalis. All exported functions they call (`fmap()`,
`fmapn()`, ...) keep their signatures and default behaviour; none of them
uses the deprecated `floop()`.
