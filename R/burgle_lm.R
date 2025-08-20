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
  formula <- (as.character(attr(object$terms, "predvars"))[-c(1:2)])

  ## interactions
  tlo <- attr(object$terms, "order")
  if(any(tlo >1)){
    tl <- attr(object$terms, "term.labels")
    tl0 <- tl[which(tlo <=1)]
    tli <- tl[which(tlo >1)]
    tli2 <- strsplit(tli, "(?<!:)(:)(?!:)", perl = T)
    formula <- c(formula, sapply(tli2, function(x) make_ints(x, o_form = formula, tl0 = tl0)))
  }

  if(attr(object$terms, "intercept") == 0) formula <- c(formula, "-1")
  ##

  l <- list("coef" = coef,
            "cov" = cov,
            "rss" = rss,
            "xlevels" = xlevels,
            "contrasts" = contrasts,
            "formula" = formula)

  class(l) <- "burgle_lm"

  l

}
