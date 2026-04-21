#' @name burgle_
#'
#' @export
burgle.cph <- function(object, ...){
  bh <- survival::basehaz(object, centered = FALSE)
  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL


  if (!is.null(object$xlevels) && (!is.null(object$strata) |
                                   any(grepl("strata", names(object$xlevels))))) {
    bh0 <- bh[, c("hazard", "strata")]
    bh <- bh[!duplicated(bh0), ]
    terms <- drop.special(terms, attr(terms, "specials")$strata)

  }
  else {
    bh <- bh[!duplicated(bh$hazard), ]
  }
  coef <- stats::coef(object)
  if (length(coef) == 0L) {
    cov <- matrix(0)
  }
  else {
    cov <- stats::vcov(object)
  }
  rss <- sum(object$residuals^2)/(sum(object$n) - length(coef))
  xlevels <- object$xlevels
  contrasts <- object$contrasts

  l <- list(coef = coef, cov = cov, rss = rss, xlevels = xlevels,
            terms = terms,
            contrasts = contrasts,
            basehaz = bh)
  class(l) <- "burgle_cph"
  l
}

#' @name predict_burgle
#'
#' @export
predict.burgle_cph <- function(object, ...){
  preds <- predict.burgle_coxph(object, ...)
  preds
}
