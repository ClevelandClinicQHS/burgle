#' Predict for burgle methods
#'
#' @rdname predict_burgle
#'
#' @param object the results of burgle_* object
#' @param newdata new data of class data.frame
#' @param original whether or not to predict using the original model
#' @param draws how many different models to simulate
#' @param sims how many simulated response to draw
#' @param type either 'lp', 'response', 'link' for glm or 'risk' if time dependent
#' @param se whether or not to include the standard error in the simulations
#' @param ... for future methods
#'
#' @return either a matrix of array of new model predictions
#' @export
#'
predict.burgle_lm <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "lp", se = FALSE, ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "response", "link"))
  nl <- names(object$xlevels)
  ck0 <- nl %in% colnames(newdata)
  if(!all(ck0)) stop(paste(nl[!ck0], "is not present in newdata"))
  ulv <- lapply(nl, function(x) unique(newdata[,x]))
  ck1s <- mapply(function(x, y) (y %in% x), object$xlevels, ulv, SIMPLIFY = FALSE)
  ck1 <- sapply(ck1s, all)

  if(length(ck1) > 0L){
    if(!all(ck1)){
      obs <- min(which(!ck1))
      stop(
        paste0("varaible ", names(object$xlevels)[obs], " has new level(s) of ", paste(ulv[[obs]][!ck1s[[obs]]], collapse = ","))
      )

    }

  }
  if(original & draws >1){
    stop("Can only have one draw from the original model")
  }
  if(original){
    models <- object$coef
  }else{
    models <- MASS::mvrnorm(n = draws, mu = object$coef, Sigma = object$cov)
  }

  mm <- stats::model.matrix(stats::reformulate(object$formula), data = newdata, xlev = object$xlevels)

  if(!is.null(dim(models))){
    preds <- apply(models, 1, function(x) mm %*% x)
  }else{
    preds <- mm %*% models
  }

  if(type == "lp"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  ## rows are observation
  ## columns are models
  ## lists are the simulations
  pn <- replicate(sims,
                  apply(preds, 2, function(x) stats::rnorm(n = length(x), mean = x, sd = ifelse(se, sqrt(object$rss), 0))),
                  simplify = FALSE)

  if(length(pn) == 1L){pn <- pn[[1]]}

  pn

}


#' @name predict_burgle
#'
#' @export
predict.burgle_glm <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "lp", se = FALSE, ...){
  type <- match.arg(tolower(type), c("lp", "response", "link"))
  preds <- predict.burgle_lm(object, newdata = newdata, original = original, draws = draws, sims = 1, type = "lp", se = FALSE,  ... = ...)
  preds <- replicate(sims,
                     apply(preds, 2, function(x) stats::rnorm(n = length(x), mean = x, sd = ifelse(se, sqrt(object$rss), 0))),
                     simplify = FALSE)
  if(length(preds) == 1L){
    preds <- preds[[1]]
  }
  if(type == "lp"){
    return(preds)
  }
  if(is.null(dim(preds))){
    preds <-  lapply(preds, object$inv_link)
  }else{
    preds <- object$inv_link(preds)
  }
  if(type == "link"){
    return(preds)
  }
  if(type == "response"){
    if(!grepl("binomial", object$family)) stop("please use type = 'link' for model families other than binomial and quasibinomial")
    if(!is.null(dim(preds))){
      pn <- apply(preds, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x))
    }else{
      pn <- lapply(preds,
                   function(y) apply(y, 2, function(x) stats::rbinom(n = length(x), size = 1, prob = x)))
    }
  }

  pn

}

