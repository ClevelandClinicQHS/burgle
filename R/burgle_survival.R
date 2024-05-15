#' @name burgle_
#'
#' @export
burgle.coxph <- function(object, ...){
  bh <- survival::basehaz(object, centered = FALSE)
  ft <- as.character(attr(object$terms, "predvars"))[-c(1:2)]
  ## intercept only model
  if(length(ft)<1){
    # formula <- stats::formula(paste0("~1"))
    formula <- "1"
  }else{
    # formula <- stats::reformulate(ft)
    formula <- ft
  }
  ## or no other coefficients

  if(!is.null(object$xlevels) && (!is.null(object$strata)| any(grepl("strata", names(object$xlevels))))){

    bh0 <- bh[, c("hazard", "strata")]
    bh <- bh[!duplicated(bh0),]
    ft <- as.character(attr(object$terms, "predvars"))[-c(1:2)]
    ft2 <- ft[!grepl("strata", ft)]
    if(length(ft2)<1){
      # formula <- stats::formula(paste0("~1"))
      formula <- "1"
    }else{
      # formula <- stats::reformulate(ft2)
      formula <- ft2
    }
  }else{
    bh <- bh[!duplicated(bh$hazard),]
  }
  coef <- stats::coef(object)
  ## 0 coefficients
  if(length(coef) == 0L){
    cov <- matrix(0)
    # coef <- 0L
  }else{
    cov <- stats::vcov(object)
  }
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

  vcovs <- mapply(function(x, y) if(length(x) == 0L){return(matrix(0))}else{stats::vcov(y)}, coefs, models, SIMPLIFY = FALSE)
  # coefs <- lapply(coefs, function(x) if(is.null(x)){return(0L)}else{x})

  formulas <- lapply(models, function(x){
    ft <- as.character(attr(x$terms, "predvars"))[-c(1:2)]
    # f1 <- stats::reformulate(ft)
    if(length(ft) <1){
      # return(stats::formula(paste0("~1")))
      return("1")
    }
    # f1 <- stats::reformulate(ft)
    f1 <- ft
    if(!is.null(x$strata)){
      ft <- as.character(attr(x$terms, "predvars"))[-c(1:2)]
      ft2 <- ft[!grepl("strata", ft)]
      if(length(ft2) <1){
        # return(stats::formula(paste0("~1")))
        return("1")
      }
      # f1 <- stats::reformulate(ft2)
      f1 <- ft2
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




