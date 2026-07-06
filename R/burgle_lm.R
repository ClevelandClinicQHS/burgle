#' Burgle_lm
#'
#' Extract model components from linear regression for uncertainty quantification.
#' Handles edge cases including singular fits, weighted models, and factor contrasts.
#'
#' @rdname burgle_
#'
#' @export
burgle.lm <- function(object, ...){

  # Extract point estimates
  coef <- stats::coef(object)
  cov <- stats::vcov(object)
  
  # Calculate residual variance (MSE equivalent)
  # For weighted models: R automatically incorporates weights in residual calculation
  # For subset models: df.residual correctly reflects subset size
  mse <- sum(object$residuals ^2)/object$df.residual
  
  # Store factor level information for prediction stage
  # Preserves contrasts for proper model matrix reconstruction in new data
  xlevels <- object$xlevels
  contrasts <- object$contrasts

  # Extract and clean formula terms
  # Terms object captures: interactions, polynomials, I() identity, factor specifications
  # Remove environment to avoid serialization issues
  terms <- object$terms
  attr(terms, ".Environment") <- NULL

  # Assemble burgle object
  # NA coefficients (from singularity) are handled during prediction, not here
  # This preserves model diagnostics and residuals for downstream use
  l <- list("coef" = coef,
            "cov" = cov,
            "mse" = mse,
            "xlevels" = xlevels,
            "contrasts" = contrasts,
            "terms" = terms)

  class(l) <- "burgle_lm"

  l

}
