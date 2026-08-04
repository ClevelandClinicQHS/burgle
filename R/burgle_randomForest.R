## burgle_randomForest.R
## randomForest::randomForest - Random forest (randomForest package)
##
## burgle.randomForest strips training-set artefacts (response vector,
## OOB predictions, vote matrix, confusion matrix, inbag counts) while
## keeping the $forest structure required for prediction.  The forest is
## only present when the model was fit with keep.forest = TRUE (the default).
##
## predict.burgle_randomForest delegates to randomForest::predict.randomForest
## so the randomForest package must be available at prediction time.

#' @name burgle_
#'
#' @export
burgle.randomForest <- function(object, ...){

  if(is.null(object$forest)){
    stop("No forest found in the randomForest object. ",
         "Refit with keep.forest = TRUE (the default).")
  }

  ## Strip training-data summaries to reduce object size
  object$y              <- NULL
  object$predicted      <- NULL
  object$votes          <- NULL
  object$oob.times      <- NULL
  object$confusion      <- NULL
  object$err.rate       <- NULL
  object$mse            <- NULL   ## regression OOB MSE vector
  object$rsq            <- NULL   ## regression OOB R-squared vector

  class(object) <- c("burgle_randomForest", "randomForest")
  object
}


#' @name predict_burgle
#'
#' @param type "response" for predicted values/classes, "risk" for class
#'   probabilities (classification only; requires the model was fit with
#'   type = "prob" via predict or fitted probability argument).
#' @param sims number of class draws for classification response simulation;
#'   ignored for regression.
#'
#' @export
predict.burgle_randomForest <- function(object, newdata = NULL, type = "response",
                                        sims = 1, ...){

  type <- match.arg(tolower(type), c("response", "risk"))

  ## Remove burgle class so randomForest::predict.randomForest dispatches
  class(object) <- class(object)[-1]

  rf_type <- object$type  ## "regression", "classification", or "unsupervised"

  ## ---- Regression ----
  if(rf_type == "regression"){
    if(type == "risk") warning("Only 'response' is available for regression forests")
    preds <- stats::predict(object, newdata = newdata, ...)
    return(preds)
  }

  ## ---- Classification ----
  if(rf_type == "classification"){
    if(type == "risk"){
      ## Return probability matrix
      probs <- stats::predict(object, newdata = newdata, type = "prob", ...)
      return(probs)
    }
    ## type == "response" - sample class labels from probability distribution
    probs <- stats::predict(object, newdata = newdata, type = "prob", ...)
    cls   <- colnames(probs)
    if(sims > 1){
      return(replicate(sims,
                       apply(probs, 1, function(p) sample(cls, size = 1, prob = p)),
                       simplify = FALSE))
    }
    return(apply(probs, 1, function(p) sample(cls, size = 1, prob = p)))
  }

  ## Fallback
  stats::predict(object, newdata = newdata, ...)
}
