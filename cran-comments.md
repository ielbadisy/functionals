## Resubmission

This is a patch release focused on progress-bar correctness, test coverage,
and CRAN-check cleanliness (a stray `LazyData` field, overfull PDF-manual
lines, and DESCRIPTION metadata).

## Test environments

- Local Linux, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 2 notes

* "unable to verify current time" — local clock-check note, unrelated to
  the package.
* "New maintainer" — the Maintainer field's family-name casing changed
  from "EL BADISY" to "El Badisy" (title case); same person, same email
  address, no change in maintainership.
