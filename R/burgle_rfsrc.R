
#' @name burgle_
#'
#' @export
burgle.rfsrc <- function(object, ...){
  if(is.null(object$forest)) stop("Set forest = TRUE in rfsrc call")
  new_rf <- object$forest
  class3 <- class(new_rf)[3]
  new_rf$xvar <- NULL
  ## This needs to be anonymized, I don't think it matters and I can't figure why it even has to be there, it only breaks a few times
  # new_rf$yvar[,] <- 1
  ## This is really makes no sense, but I think it works since the newdata is always a test dataset.
  if(class3 %in% c("surv", "surv-CR")){
    new_rf$yvar <- new_rf$yvar[1,]
    new_rf$yvar[,] <- 1
  }else{
    new_rf$yvar <- NULL
  }
  new_rf$n <- 1
  ###
  new_rf$forest <- NULL
  new_rf$event.info$time <- NULL
  new_rf$event.info$event <- NULL
  ## I think this works for now, it saves space
  new_rf$nativeArrayTNDS$tnRMBR <- NULL
  new_rf$nativeArrayTNDS$tnAMBR <- NULL
  ### above this
  new_rf$event.info$cens <- unique(new_rf$event.info$cens)
  new_rf$seed <- NULL
  remove(list = ls(environment(new_rf$sampsize)), envir = environment(new_rf$sampsize))
  classes <- attr(new_rf, "class")
  if (sum(grepl("quantreg", class(object))) > 0){
    new_rf$yvar.grow <- object$yvar.grow
    new_rf$res.grow <- object$res.grow
    classes <- c(classes, "quantreg")
  }
  ### this might be the way to do things in the future
  ### we'll get creative

  classes <- c("burgle_rfsrc", classes)
  attr(new_rf, "class") <- classes
  return(new_rf)
}

#' @name predict_burgle
#'
#' @export
predict.burgle_rfsrc <- function(object, newdata = NULL, type = "risk", sims = 1, cause = NA, times = NA, ...){

  type <- match.arg(tolower(type), c("response", "risk"))

  # if(type == "importance"){
  #   importance = TRUE
  # }else{
  #   importance = FALSE
  # }
  attr(object, "class") <- attr(object, "class")[-1]
  op1 <- stats::predict(object, newdata = newdata, importance = F, ...)

  class3 <- class(op1)[3]

  # if(type == "importance"){
  #   return(op1$importance)
  # }

  if(class3 == "regr"){
    if(type == "risk"){warning("Only response is available for regression trees")}
    p <- op1$predicted
    return(p)
  }
  if(class3 == "class"){
    odds <- op1$predicted
    if(type == "risk"){
      return(odds)
    }

    if(sims > 1){
      p <- replicate(sims, apply(odds, 1, function(x) sample(colnames(odds), prob = x, size = 1)), simplify = FALSE)
      return(p)
    }else{
      p <- apply(odds, 1, function(x) sample(colnames(odds), prob = x, size = 1))
    }
    return(p)

  }
  if(class3 == "surv"){

    odds <- 1- op1$survival

  }
  if(class3 == "surv-CR"){
    if(is.na(cause) | is.na(times)) stop("Please specificy a cause and time")

    odds <- op1$cif[, , cause, drop = TRUE]
  }
  pos <- prodlim::sindex(jump.times = object$time.interest,
                         eval.times = times)
  p <- cbind(0, odds)[, pos + 1, drop = FALSE]


  if(type == "risk"){
    return(p)
  }
  if(sims > 1){
    p <- replicate(sims, apply(p, 2, function(x) stats::rbinom(n = length(x), prob = x, size = 1)), simplify = FALSE)
    return(p)
  }else{
    p <- apply(p, 2, function(x) stats::rbinom(n = length(x), prob = x, size = 1))
  }
  return(p)

}

## regression "regr"
## classification "class"
## multivariate
## survival "surv"
## competiting risk "surv-CR"


# f1 <- burgle.rfsrc(wihs.obj)
#
# f2 <- wihs.obj$forest
# f2$xvar <- NULL
# f2$yvar <- NULL
# f2$forest <- NULL
#
# attr(f2, "class") <- c('rfsrc', 'forest', 'surv-CR', 'burgle_rfsrc')
# # f2$leafCount <- NULL
# # f2$sampsize <- NULL
# # f3 <- environment(f2$sampsize)
# remove(list = ls(environment(f2$sampsize)), envir = environment(f2$sampsize))
#
#
# cif <- stats::predict(f1, newdata = head(wihs),
#                       importance = "none")$cif[, , 1, drop = TRUE]

###
# new_rf <- crf4$forest
#
# new_rf <- object$forest
# new_rf$xvar <- NULL
# ## I think this needs to simply be anonymized
# new_rf$yvar[,] <- 1
# new_rf$forest <- NULL
# new_rf$event.info$time <- NULL
# new_rf$event.info$event <- NULL
# new_rf$event.info$cens <- unique(new_rf$event.info$cens)
# new_rf$nativeArrayTNDS$tnRMBR <- NULL
# new_rf$nativeArrayTNDS$tnAMBR <- NULL
# new_rf$seed <- NULL
# remove(list = ls(environment(new_rf$sampsize)), envir = environment(new_rf$sampsize))
