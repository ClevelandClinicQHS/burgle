#' Burgle_lm
#'
#' @rdname burgle_
#'
#' @export
burgle.lm <- function(object, ...){

  coef <- stats::coef(object)
  cov <- stats::vcov(object)
  
  if(any(is.na(coef))){
    na_names <- names(coef)[is.na(coef)]
    warning("Coefficient(s) NA in linear model: ", paste(na_names, collapse=", "), 
            ". These will be replaced with 0. This typically occurs due to multicollinearity or singularities in the design matrix.")
    coef[is.na(coef)] <- 0
    cov[is.na(cov)] <- 0
  }
  
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
