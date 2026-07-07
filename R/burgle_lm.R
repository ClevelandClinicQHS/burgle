#' Burgle_lm
#'
#' @rdname burgle_
#'
#' @export
burgle.lm <- function(object, ...){

  coef <- stats::coef(object)
  cov <- stats::vcov(object)

  if(any(is.na(coef))){
    warning("At least 1 coefficient has a value of NA")
    coef[is.na(coef)] <- 0
    cov[is.na(cov)] <- 0
  }

  mse <- sum(object$residuals ^2)/object$df.residual
  xlevels <- object$xlevels
  contrasts <- object$contrasts

  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  l <- list("coef" = coef,
            "cov" = cov,
            "mse" = mse,
            "xlevels" = xlevels,
            "contrasts" = contrasts,
            "terms" = terms)

  class(l) <- "burgle_lm"

  l

}
