## burgle_rlm.R
## MASS::rlm - M-type robust regression (MASS package)
##
## burgle.rlm extracts the same components as burgle.lm but uses the robust
## scale estimate (object$s) in place of the OLS residual MSE. The resulting
## burgle_rlm object inherits from burgle_lm so predict.burgle_lm and
## simulate_models.burgle_lm are reused automatically.

#' @name burgle_
#'
#' @export
burgle.rlm <- function(object, ...){

  coef <- stats::coef(object)
  cov  <- stats::vcov(object)

  ## Robust scale estimate: s^2 replaces the OLS MSE.
  ## object$s is the robust estimate of the residual standard deviation.
  mse <- object$s ^ 2

  xlevels  <- object$xlevels
  contrasts <- object$contrasts

  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  l <- list("coef"      = coef,
            "cov"       = cov,
            "mse"       = mse,
            "xlevels"   = xlevels,
            "contrasts" = contrasts,
            "terms"     = terms)

  ## Inherit predict.burgle_lm and simulate_models.burgle_lm via class vector
  class(l) <- c("burgle_rlm", "burgle_lm")
  l
}
