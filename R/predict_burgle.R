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
predict.burgle_lm <- function(object, newdata = NULL, original = TRUE, draws = 1, sims = 1, type = "lp", se = FALSE, limits = NULL, seed = NULL, se_type = "prediction", ...){
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
predict.burgle_glm <- function(object, newdata = NULL, original = TRUE, draws = 1, sims = 1, type = "lp", se = FALSE, limits = NULL, seed = NULL, se_type = "prediction", ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "response", "link"))

  if(original & draws > 1){
    stop("Can only have one draw from the original model")
  }

  models <- draw_models(object, original = original, draws = draws, seed = seed)

  pn <- simulate_models(object, models = models, newdata = newdata, sims = sims, type = type, se = se, limits = limits, seed = seed, se_type = se_type, ...)

  pn

}

#' Predict for burgle methods
#'
#' @rdname predict_burgle
draw_models <- function(object, original = T, draws = 1, seed= NULL){
  if (is.null(object$coef) || length(object$coef) == 0L) {
    if (original) {
      return(0L)
    }
    if (draws <1|is.na(draws)){stop("draws must be at least 1")}
    return(matrix(0, nrow = draws))
  }

  if (original){
      models <- object$coef
  } else {
    if (draws <1|is.na(draws)){stop("draws must be at least 1")}
    set.seed(seed = seed)
    models <- MASS::mvrnorm(n = draws, mu = object$coef, Sigma = object$cov)
  }

  if (!is.null(dim(models))){
    # Multiple models case (draws > 1)
    na_mask <- is.na(models)
    if (any(na_mask)){
      na_names <- colnames(models)[colSums(na_mask) > 0]
      warning("Coefficient(s) NA in model: ", paste(na_names, collapse = ", "),
              ". These will be replaced with 0 during prediction. This typically occurs due to multicollinearity or singularities in the design matrix.")
      models[na_mask] <- 0
    }
  } else {
    # Single model case (original = TRUE or draws = 1)
    na_mask <- is.na(models)
    if (any(na_mask)){
      na_names <- names(models)[na_mask]
      warning("Coefficient(s) NA in model: ", paste(na_names, collapse = ", "),
              ". These will be replaced with 0 during prediction. This typically occurs due to multicollinearity or singularities in the design matrix.")
      models[na_mask] <- 0
    }
  }

  return(models)
}

#' @rdname simulate_models
#' @param se whether or not to include the standard error in the simulations
#' @param limits limits (minimum and maximum) for simulated response values
#' @param se_type either 'prediction' or 'confidence' for standard errors use in simulating
#'
#' @export
simulate_models.burgle_lm <- function(object, models = NULL, newdata = NULL, type = "lp", sims =1, seed = NULL, se = FALSE, limits = NULL, se_type = "prediction", ...){
  if(is.null(models)) stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")

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
simulate_models.burgle_glm <- function(object, models = NULL, newdata = NULL, type = "lp", sims = 1, seed = NULL, se = FALSE, limits = NULL, se_type = "prediction", ...){
  if(is.null(models)) stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")

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

  ## Simulate on the linear predictor (link) scale
  if(is.null(limits)){
    preds <- simulate_responses(preds, sims, se, se_p)
  } else {
    preds <- simulate_responses_limits(preds, sims, se, se_p, limits = limits)
  }

  if(type == "link"){
    ## Return simulated values on the linear predictor (link) scale
    return(preds)
  }

  ## type == "response": apply inverse link and return response-scale predictions
  preds <- lapply(preds, object$inv_link)

  if(grepl("binomial", object$family)){
    ## For binomial, sample binary outcomes from the probabilities
    if(is.list(preds)){
      pn <- lapply(preds, simulate_responses_binom, sims = 1)
    } else {
      pn <- simulate_responses_binom(preds, sims = 1)
    }
    pn <- lapply(pn, drop_list)
    pn <- drop_list(pn)
  } else {
    ## For non-binomial, return the response-scale (mean) predictions
    pn <- drop_list(preds)
  }

  return(pn)

}

drop_list <- function(x){
  if(is.list(x) & length(x) == 1L) x <- x[[1]]
  return(x)
}



