#' @name burgle_
#'
#' @export
burgle.cph <- function(object, ...){
  bh <- survival::basehaz(object, centered = FALSE)
  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL


  has_strata <- !is.null(object$strata) || "strata" %in% colnames(bh)
  if (has_strata) {
    bh0 <- bh[, c("hazard", "strata")]
    bh <- bh[!duplicated(bh0), ]
    terms <- strip_strata_terms(terms)

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

#' @name predict_burgle
#'
#' @export
simulate_models.burgle_cph <- function(object,  ...){
  sims <- simulate_models.burgle_coxph(object, ...)
  sims
}
