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
#' @param se_type either 'prediction' or 'confidence' for standard errors use in simulating
#' @param limits limits (minimum and maximum) for simulated response values.
#' @param seed seed to set for reproducibility
#' @param ... for future methods
#'
#' @return either a matrix or list of new model predictions
#' @export
#'
predict.burgle_lm <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "lp", se = FALSE, limits = NULL, seed = NULL, se_type = "prediction", ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "response", "link"))

  if(original & draws >1){
    stop("Can only have one draw from the original model")
  }

  models <- draw_models(object, original = original, draws = draws, seed = seed)

  pn <- simulate_models(object, models = models, newdata = newdata, sims = sims, type = type, se = se, limits = limits, seed = seed, se_type = se_type, ...)

  pn

}


#' @name predict_burgle
#'
#' @export
predict.burgle_glm <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "lp", se = FALSE, seed = NULL, ...){
  type <- match.arg(tolower(type), c("lp", "response", "link"))

  models <- draw_models(object, original = original, draws = draws, seed = seed)

  pn <- simulate_models(object, models = models, newdata = newdata, sims = sims, type = type, se = se, seed = seed, se_type = "prediction", ...)

  # preds <- predict.burgle_lm(object, newdata = newdata, original = original, draws = draws, sims = sims, type = type, se = se, ...)

  pn

}

rsamp <- function(FUN, limits, ...){
  dots <- list(...)
  l <- formals(FUN)
  if(!("n" %in% names(l))){
    stop("sample size (n) not present in called FUN")
  }
  y <- do.call(FUN, dots)

  if(length(limits)!=2L){
    stop("Limits must be of length two")
  }
  mn <- min(limits)
  mx <- max(limits)
  i <- 1
  while(any(y<mn|y>mx)){
    n1 <- sum(y<mn|y>mx)
    dots["n"] <- n1
    i
    wn1 <- which(y<mn|y>mx)
    ## This has to change the x changes
    dots2 <- lapply(dots, function(x) if(length(x)>1L) x[wn1] else x)
    y[wn1] <- do.call(FUN, dots2)

  }
  return(y)
}

#' Predict for burgle methods
#'
#' @rdname predict_burgle
draw_models <- function(object, original = T, draws = 1, seed= NULL){
  if(original){
    models <- object$coef
  }else{
    if(draws <1|is.na(draws)){stop("draws must be at least 1")}
    set.seed(seed = seed)
    models <- MASS::mvrnorm(n = draws, mu = object$coef, Sigma = object$cov)
  }
  return(models)
}

#' @rdname simulate_models
#' @param se whether or not to include the standard error in the simulations
#' @param limits limits (minimum and maximum) for simulated response values
#' @param se_type either 'prediction' or 'confidence' for standard errors use in simulating
#'
#' @export
simulate_models.burgle_lm <- function(object, models = NULL, newdata, sims =1, type = "lp", se = FALSE, limits = NULL, seed = NULL, se_type = "prediction", ...){
  if(is.null(models)) stop("Please specificy models using `draw_models()`, otherwise use corresponding predict()")

  mm <- stats::model.matrix(object$terms, data = newdata, xlev = object$xlevels, contrasts.arg = object$contrasts)

  if(!is.null(dim(models))){
    preds <- fastmm(mm, t(models))
  }else{
    preds <- fastmm(mm, matrix(models))
  }

  if(type == "lp"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  se_p <- rowSums(fastmm(mm, object$cov) * mm)
  if(se_type == "prediction") se_p <- sqrt(se_p + object$mse)

  ## rows are observation
  ## columns are models
  ## lists are the simulations
  if(is.null(limits)){

    pn <- simulate_responses(preds, sims, se, se_p)
  }else{

    pn <- simulate_responses_limits(preds, sims, se, se_p, limits = limits)
  }

  # if(length(pn) == 1L){pn <- pn[[1]]}
  pn <- drop_list(pn)


  pn


}

#' @rdname simulate_models
#'
#' @export
simulate_models.burgle_glm <- function(object, models = NULL, newdata, sims =1, type = "lp", se = FALSE, seed = NULL, se_type = "prediction", ...){
  if(is.null(models)) stop("Please specificy models using `draw_models()`, otherwise use corresponding predict()")

  mm <- stats::model.matrix(object$terms, data = newdata, xlev = object$xlevels, contrasts.arg = object$contrasts)

  if(!is.null(dim(models))){
    preds <- fastmm(mm, t(models))
  }else{
    preds <- fastmm(mm, matrix(models))
  }

  if(type == "lp"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  se_p <- rowSums(fastmm(mm, object$cov) * mm)
  if(se_type == "prediction") se_p <- sqrt(se_p + object$mse)

  ## rows are observation
  ## columns are models
  ## lists are the simulations

  preds <- simulate_responses(preds, sims, se, se_p)

  preds <-  lapply(preds, object$inv_link)

  if(type == "link"){
    return(preds)
  }
  if(type == "response"){
    if(!grepl("binomial", object$family)) stop("please use type = 'link' for model families other than binomial and quasibinomial")
    if(is.list(preds)){
      pn <- lapply(preds, simulate_responses_binom, sims = 1)
    }
    else{
      pn <- simulate_responses_binom(preds, sims = 1)
    }

  }

  pn <- lapply(pn, drop_list)
  pn <- drop_list(pn)

  return(pn)


}

drop_list <- function(x){
  if(is.list(x) & length(x) == 1L) x <- x[[1]]
  return(x)
}



