#' @name burgle_
#'
#' @export
burgle.coxph <- function(object, ...){
  bh <- survival::basehaz(object, centered = FALSE)
  formula <- stats::reformulate(as.character(attr(object$terms, "predvars"))[-c(1:2)])
  if(!is.null(object$strata)){
    bh0 <- bh[, c("hazard", "strata")]
    bh <- bh[!duplicated(bh0),]
    ft <- as.character(attr(object$terms, "predvars"))[-c(1:2)]
    formula <- stats::reformulate(ft[!grepl("strata", ft)])

  }else{
    bh <- bh[!duplicated(bh$hazard),]
  }
  coef <- stats::coef(object)
  cov <- stats::vcov(object)
  rss <- sum(object$residuals ^2)/(object$n - length(coef))
  xlevels <- object$xlevels
  l <- list("coef" = coef,
            "cov" = cov,
            "rss" = rss,
            "xlevels" = xlevels,
            "formula" = formula,
            "basehaz" = bh)
  class(l) <- "burgle_coxph"
  l
}

#' @name burgle_
#'
#' @export
burgle.CauseSpecificCox <- function(object, ...){

  models <- object$models
  evtimes <- object$eventTimes
  Net <- length(evtimes)

  basehzs <- lapply(models, function(x) riskRegression::predictCox(x, centered = FALSE,
                                                                   times = evtimes, newdata = NULL, type = c("hazard", "cumhazard"), keep.strata = TRUE, keep.times = TRUE,
                                                                   se = FALSE, keep.infoVar = TRUE))

  cumhazards <- lapply(basehzs, function(x) matrix(x[["cumhazard"]], byrow = FALSE, nrow = Net))
  hazards <- lapply(basehzs, function(x) matrix(x[["hazard"]], byrow = FALSE, nrow = Net))
  levT <- lapply(basehzs, `[[`, "lastEventTime")
  coefs <- lapply(models, stats::coef)
  vcovs <- lapply(models, stats::vcov)

  formulas <- lapply(models, function(x){
    f1 <- stats::reformulate(as.character(attr(x$terms, "predvars"))[-c(1:2)])
    if(!is.null(x$strata)){
      ft <- as.character(attr(x$terms, "predvars"))[-c(1:2)]
      f1 <- stats::reformulate(ft[!grepl("strata", ft)])
    }
    f1
  })

  evtimes <- object$eventTimes

  xlevels <- lapply(object$models, `[[`, "xlevels")

  l <- list("cumhazards" = cumhazards,
            "hazards" = hazards,
            "eventTimes"  = evtimes,
            "Lastevent" = levT,
            "coef" = coefs,
            "cov" = vcovs,
            "xlevels" = xlevels,
            "formulas" = formulas
  )

  class(l) <- "burgle_CauseSpecificCox"
  l

}




