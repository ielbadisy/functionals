# functionals 0.5.1

- Fixed `fapply()` and `fmap()` progress reporting so sequential, multicore, and
  cluster-backed execution advance on completed tasks rather than internal
  chunk boundaries.
- Improved progress-bar output stability when user code emits messages during
  tracked execution.
- Added progress-system regression tests covering sequential timing, exact
  parallel counts, and error propagation in cluster and multicore execution.
- Updated package documentation to describe completion-driven progress
  semantics.
