#' Title
#'
#' @param object a burgle object, used in with survival time
#' @param ...
#'
#' @returns a dataframe of with a smple
#' @export
#'
#' @examples
#' library(survival)
#' lung <- survival::lung
#' cx <- burgle(coxph(Surv(time, status-1)~age + sex, data = lung))
#' predict_time(cx, newdata = head(lung))
predict_time <- function(object, ...){
  UseMethod("predict_time")
}

#' @export
predict_time.burgle_CauseSpecificCox <- function(object, newdata, ...){

  lps <- predict(object, newdata = newdata, type = "lp", ...)

  ## grab hazards from the CSC object
  chs <- lapply(object$hazards, function(x) data.frame(time = object$eventTimes, Haz = x[,1]))

  ## individualizes the hazards
  chst <- update_hazards(lps, chs)

  ## samples time and event
  ste <- sample_time_event(chst)

  return(ste)

}
#' @name predict_burgle
#'
#' @export
predict_time.burgle_coxph <- function(object, newdata, ...){

  lps <- predict(object, newdata = newdata, type = "lp", ...)

  bh <- object$basehaz
  bh$hazard <- diff(c(0, bh$hazard))
  colnames(bh)[1] <- "Haz"

  chs <- update_hazards(lps, list(bh))

  ste <- sample_time_event(chs)

  return(ste)

}

#' @name predict_burgle
#'
#' @export
predict_time.burgle_flexsurvreg <- function(object, newdata, ...){

  ste <- predict(object, newdata = newdata, type = "time", ...)

  return(ste)

}
