## Development version

* Add `burgle.workflow()` for fitted tidymodels workflows. Trained `step_ns()`
  expansions become explicit `splines::ns()` terms with fixed knots and
  boundaries, usable for prediction and refitting without recalculating knots.
  Other recipe steps warn and are not reconstructed. Partial or interacting
  spline expansions are rejected.

# Changes in version 0.1.2

## New features

* Now allows ordered factor terms
* burgle methods now exist for lme4::lmer models

# Changes in version 0.1.1

## New features

* Now allows zero-intercept models and will incorporate interaction terms
* burgle methods now exist for nnet::multinom models

## Minor improvements

* Set the default to original = TRUE for all predict methods

# burgle 0.1.0

* Initial CRAN submission.
