## burgle_ranger.R
## ranger::ranger - Fast random forest (ranger package)
##
## The ranger forest object is self-contained for prediction (the training data
## is not stored). burgle.ranger only removes components that are purely
## training-set summaries (OOB predictions, confusion matrix, inbag counts) to
## shrink the object, while keeping the forest structure required by
## ranger::predict.ranger.
##
## predict.burgle_ranger strips the burgle class and delegates directly to
## ranger::predict.ranger, so the ranger package must be available at
## prediction time.

#' @name burgle_
#'
#' @export
burgle.ranger <- function(object, ...){

  if(!inherits(object, "ranger")){
    stop("object must be of class 'ranger'")
  }

  ## Strip training-set summaries to reduce size
  object$predictions      <- NULL
  object$confusion.matrix <- NULL
  object$inbag.counts     <- NULL

  ## Tag the object
  class(object) <- c("burgle_ranger", "ranger")
  object
}


#' @name predict_burgle
#'
#' @param type "response" for predicted values/classes, "risk" for class
#'   probabilities (classification) or 1 - survival (survival forests).
#'   For regression forests "risk" is not meaningful; "response" is returned
#'   with a warning.
#' @param times (survival only) evaluation time points for risk/response.
#' @param sims number of class draws for classification response simulation;
#'   ignored for regression.
#'
#' @export
predict.burgle_ranger <- function(object, newdata = NULL, type = "response",
                                  times = NULL, sims = 1, ...){

  type <- match.arg(tolower(type), c("response", "risk"))

  ## Remove the burgle class so ranger::predict.ranger dispatches correctly
  class(object) <- class(object)[-1]

  pred <- stats::predict(object, data = newdata, ...)

  tree_type <- object$treetype

  ## ---- Regression ----
  if(tree_type == "Regression"){
    if(type == "risk") warning("Only 'response' is available for regression forests")
    return(pred$predictions)
  }

  ## ---- Classification ----
  if(tree_type %in% c("Classification", "Probability estimation")){
    if(type == "risk"){
      ## Return probability matrix (requires probability = TRUE in ranger call)
      if(is.null(pred$predictions) || is.null(dim(pred$predictions))){
        warning("Probability matrix not available; refit with probability = TRUE in ranger()")
        return(pred$predictions)
      }
      return(pred$predictions)
    }
    ## type == "response"
    if(!is.null(dim(pred$predictions))){
      ## probability forest - sample class labels
      if(sims > 1){
        cls <- colnames(pred$predictions)
        return(replicate(sims,
                         apply(pred$predictions, 1, function(p) sample(cls, size = 1, prob = p)),
                         simplify = FALSE))
      }
      cls <- colnames(pred$predictions)
      return(apply(pred$predictions, 1, function(p) sample(cls, size = 1, prob = p)))
    }
    return(pred$predictions)
  }

  ## ---- Survival ----
  if(tree_type == "Survival"){
    ## pred$survival is n x T matrix; pred$unique.death.times is the time grid
    surv_mat  <- pred$survival
    eval_times <- pred$unique.death.times

    if(!is.null(times)){
      ## Find the column indices closest to (or <= ) the requested times
      pos <- prodlim::sindex(jump.times = eval_times, eval.times = times)
      risk_mat <- cbind(0, 1 - surv_mat)[, pos + 1, drop = FALSE]
    } else {
      risk_mat <- 1 - surv_mat
    }

    if(type == "risk") return(risk_mat)

    ## type == "response" - simulate binary event outcome
    if(sims > 1){
      return(replicate(sims,
                       apply(risk_mat, 2, function(p) stats::rbinom(length(p), 1, p)),
                       simplify = FALSE))
    }
    return(apply(risk_mat, 2, function(p) stats::rbinom(length(p), 1, p)))
  }

  ## Fallback
  pred$predictions
}
