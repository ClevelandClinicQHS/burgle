#' @name burgle_
#'
#' @export
burgle.cph <- function(object, ...){
  bh <- survival::basehaz(object, centered = FALSE)
  ft <- as.character(attr(object$terms, "variables"))[-c(1:2)]
  if (length(ft) < 1) {
    formula <- "1"
  }
  else {
    formula <- ft
  }
  tlo <- attr(object$terms, "order")
  if (any(tlo > 1)) {
    tl <- attr(object$terms, "term.labels")
    tl0 <- tl[which(tlo <= 1)]
    tli <- tl[which(tlo > 1)]
    tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
    formula <- c(formula, sapply(tli2, function(x) make_ints(x,
                                                             o_form = formula, tl0 = tl0)))
  }
  if (!is.null(object$xlevels) && (!is.null(object$strata) |
                                   any(grepl("strata", names(object$xlevels))))) {
    bh0 <- bh[, c("hazard", "strata")]
    bh <- bh[!duplicated(bh0), ]
    ft <- as.character(attr(object$terms, "predvars"))[-c(1:2)]
    ft2 <- ft[!grepl("strata", ft)]
    if (length(ft2) < 1) {
      formula <- "1"
    }
    else {
      formula <- ft2
    }
    tlo <- attr(object$terms, "order")
    if (any(tlo > 1)) {
      tl <- attr(object$terms, "term.labels")
      tl0 <- tl[which(tlo <= 1)]
      tli <- tl[which(tlo > 1)]
      tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
      formula <- c(formula, sapply(tli2, function(x) make_ints(x,
                                                               o_form = formula, tl0 = tl0)))
    }
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
  l <- list(coef = coef, cov = cov, rss = rss, xlevels = xlevels,
            formula = formula, basehaz = bh)
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
