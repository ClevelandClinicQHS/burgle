#' Burgle_lm
#'
#' @rdname burgle_
#'
#' @export
burgle.lm <- function(object, ...){

  coef <- stats::coef(object)
  cov <- stats::vcov(object)
  rss <- sum(object$residuals ^2)/object$df.residual
  xlevels <- object$xlevels
  contrasts <- object$contrasts

  terms <- object$terms
  attr(terms, ".Environment") <- NULL

  l <- list("coef" = coef,
            "cov" = cov,
            "rss" = rss,
            "xlevels" = xlevels,
            "contrasts" = contrasts,
            "terms" = terms)

  class(l) <- "burgle_lm"

  l

}
