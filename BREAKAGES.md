# Breakage Report

This document records where the test suite was breaking in this environment and what was changed.

## Confirmed break points

1. `tests/testthat/test-burgle-survreg.R`
   - Failure: `predict.burgle_survreg lp matches survreg linear predictor`
   - Error: `Rcpp::exception: Incompatible matrix dimensions`
   - Root cause: `burgle.survreg()` stored only regression coefficients, but `vcov.survreg()` includes `Log(scale)`. This caused index/matrix mismatches in prediction.
   - Fix: `R/burgle_survreg.R` now stores a full coefficient vector aligned to `vcov` (including `Log(scale)` for non-exponential models), and keeps `loc_idx` / `scale_idx` consistent.

2. `tests/testthat/test-burgle-clm.R`
   - Failure: comparison to `ordinal::predict.clm()` failed with `non-conformable arguments`.
   - Root cause: the original fixture (`iris`) with response included in `newdata` triggered version-specific issues in `predict.clm`.
   - Fix: tests now use the stable `MASS::housing` ordinal dataset and pass predictor-only `newdata`.

3. `tests/testthat/test-burgle-polr.R`
   - Failure: multiple tests errored at fit creation with `attempt to find suitable starting values failed`.
   - Root cause: original `iris` fixture/formula was unstable for `MASS::polr` in this environment/version.
   - Fix: tests now use `MASS::housing` with an ordered response and predictor-only `newdata`.

## Notes on skipped tests

Some tests remain skipped when optional packages are not installed (`betareg`, `fixest`, `flexsurv`, `parsnip`, `randomForestSRC`, and specific edge-case skip markers already present in the suite). These are existing conditional skips, not new breakages.
