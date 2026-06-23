#' @rdname burgle_
#'
#' @param lambda for glmnet, either "min" (default) or a specific lambda value
#'
#' @export
burgle.glmnet <- function(object, lambda = "min", ...){
  # Determine lambda value to use
  if(is.character(lambda)){
    lambda <- tolower(lambda)
    if(lambda == "min"){
      lambda_val <- object$lambda.min
    }else{
      stop(paste("Unknown lambda choice:", lambda))
    }
  }else{
    lambda_val <- lambda
  }
  
  # Extract coefficients at chosen lambda
  coef <- as.numeric(coef(object, s = lambda_val)[-1])
  
  # glmnet does not provide covariance matrix - set to zero matrix
  # This means draw_models will only return the original coefficients
  cov <- matrix(0, nrow = length(coef), ncol = length(coef))
  
  # Get family information
  family <- object$call$family
  if(is.null(family)) family <- "gaussian"
  
  # Get inverse link function based on family
  inv_link <- get_inv_link(family)
  
  # For gaussian, use MSE from model; for others, set to 0
  mse <- 0
  
  l <- list(
    "coef" = coef,
    "cov" = cov,
    "mse" = mse,
    "lambda_value" = lambda_val,
    "family" = family,
    "nfeatures" = nrow(object$beta),
    "inv_link" = inv_link
  )
  
  class(l) <- "burgle_glmnet"
  
  l
}

#' @rdname burgle_
#'
#' @param lambda for cv.glmnet, either "lambda.1se" (default), "lambda.min", or a specific lambda value
#'
#' @export
burgle.cv.glmnet <- function(object, lambda = "lambda.1se", lambda_choice = NULL, ...){
  # Support backward compatibility for lambda_choice parameter
  if(!is.null(lambda_choice)){
    warning("lambda_choice is deprecated, use lambda instead")
    lambda <- lambda_choice
  }
  
  # Get the chosen lambda
  if(is.character(lambda)){
    lambda <- tolower(lambda)
    if(lambda == "lambda.1se"){
      lambda_val <- object$lambda.1se
      lambda_choice <- "lambda.1se"
    }else if(lambda == "lambda.min"){
      lambda_val <- object$lambda.min
      lambda_choice <- "lambda.min"
    }else{
      stop(paste("Unknown lambda choice:", lambda))
    }
  }else{
    lambda_val <- lambda
    lambda_choice <- NA_character_
  }
  
  if(is.null(lambda_val)){
    stop(paste("lambda choice '", lambda, "' not found in cv.glmnet object", sep=""))
  }
  
  # Extract coefficients at the chosen lambda
  coef <- as.numeric(coef(object, s = lambda_val)[-1])
  
  # glmnet does not provide covariance matrix - set to zero matrix
  cov <- matrix(0, nrow = length(coef), ncol = length(coef))
  
  # Get family information
  family <- object$call$family
  if(is.null(family)) family <- "gaussian"
  
  # Get inverse link function based on family
  inv_link <- get_inv_link(family)
  
  # For gaussian, use MSE from model; for others, set to 0
  mse <- 0
  
  l <- list(
    "coef" = coef,
    "cov" = cov,
    "mse" = mse,
    "lambda_value" = lambda_val,
    "lambda_choice" = lambda_choice,
    "family" = family,
    "nfeatures" = nrow(object$glmnet.fit$beta),
    "cv_object" = object,
    "inv_link" = inv_link
  )
  
  class(l) <- "burgle_cv.glmnet"
  
  l
}

# Internal helper function to get inverse link function from family specification
get_inv_link <- function(family){
  if(is.character(family)){
    family <- tolower(family)
  }
  
  if(grepl("gaussian", family)){
    inv_link <- function(x) x
  }else if(grepl("binomial", family)){
    inv_link <- function(x) 1 / (1 + exp(-x))
  }else if(grepl("poisson", family)){
    inv_link <- function(x) exp(x)
  }else if(grepl("multinomial", family)){
    # Softmax for multinomial
    inv_link <- function(x) {
      if(is.list(x)){
        lapply(x, function(xi) {
          exp(xi) / rowSums(exp(xi))
        })
      }else if(is.matrix(x)){
        exp(x) / rowSums(exp(x))
      }else{
        exp(x) / sum(exp(x))
      }
    }
  }else{
    stop(paste("Family", family, "not supported"))
  }
  
  return(inv_link)
}

# Internal function for drawing models from glmnet objects
# For glmnet, we don't have a covariance matrix, so we just return the original coefficients
#' @export
draw_models.burgle_glmnet <- function(object, original = TRUE, draws = 1, seed = NULL){
  if(original){
    models <- object$coef
  }else{
    # For glmnet, we can't sample from a zero covariance matrix
    # Instead, just return the original coefficients multiple times
    if(draws < 1 | is.na(draws)) stop("draws must be at least 1")
    models <- matrix(object$coef, nrow = draws, ncol = length(object$coef), byrow = TRUE)
  }
  return(models)
}

#' @export
draw_models.burgle_cv.glmnet <- function(object, original = TRUE, draws = 1, seed = NULL){
  # cv.glmnet objects use the same logic as glmnet objects
  if(original){
    models <- object$coef
  }else{
    # For glmnet, we can't sample from a zero covariance matrix
    # Instead, just return the original coefficients multiple times
    if(draws < 1 | is.na(draws)) stop("draws must be at least 1")
    models <- matrix(object$coef, nrow = draws, ncol = length(object$coef), byrow = TRUE)
  }
  return(models)
}

#' Draw Models
#'
#' Generic function to draw models from burgle objects
#'
#' @param object a burgle object
#' @param original whether to return the original model
#' @param draws number of draws to generate
#' @param seed random seed for reproducibility
#'
#' @export
draw_models <- function(object, original = TRUE, draws = 1, seed = NULL){
  UseMethod("draw_models")
}

#' @name predict_burgle
#'
#' @export
predict.burgle_glmnet <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "response", se = FALSE, seed = NULL, ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("link", "response"))
  
  if(original & draws > 1){
    stop("Can only have one draw from the original model")
  }
  
  models <- draw_models(object, original = original, draws = draws, seed = seed)
  
  pn <- simulate_models(object, models = models, newdata = newdata, sims = sims, type = type, se = se, seed = seed, ...)
  
  pn
}

#' @name predict_burgle
#'
#' @export
predict.burgle_cv.glmnet <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "response", se = FALSE, seed = NULL, ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("link", "response"))
  
  if(original & draws > 1){
    stop("Can only have one draw from the original model")
  }
  
  models <- draw_models(object, original = original, draws = draws, seed = seed)
  
  pn <- simulate_models(object, models = models, newdata = newdata, sims = sims, type = type, se = se, seed = seed, ...)
  
  pn
}

#' @rdname simulate_models
#'
#' @export
simulate_models.burgle_glmnet <- function(object, models = NULL, newdata, type = "response", sims = 1, seed = NULL, se = FALSE, ...){
  if(is.null(models)) stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  
  # Convert newdata to matrix for prediction
  # Remove response variable if present
  mm <- as.matrix(newdata)
  if(ncol(mm) != length(object$coef)){
    stop(paste("Number of features in newdata (", ncol(mm), ") does not match model features (", length(object$coef), ")", sep=""))
  }
  
  # Compute linear predictor: X %*% coef
  if(!is.null(dim(models))){
    preds <- fastmm(mm, t(models))
  }else{
    preds <- fastmm(mm, matrix(models))
  }
  
  if(type == "link"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'link'")
    return(preds)
  }
  
  # For response type, apply inverse link based on family
  if(type == "response"){
    # Apply inverse link
    if(is.list(preds)){
      pn <- lapply(preds, object$inv_link)
    }else{
      pn <- object$inv_link(preds)
    }
    
    family <- object$family
    if(is.character(family)) family <- tolower(family)
    
    # Simulate binomial responses if needed
    if(grepl("binomial", family)){
      if(is.list(pn)){
        pn <- lapply(pn, simulate_responses_binom, sims = 1)
      }else{
        pn <- simulate_responses_binom(pn, sims = 1)
      }
      
      pn <- lapply(pn, drop_list)
      pn <- drop_list(pn)
      return(pn)
    }
    
    return(drop_list(pn))
  }
  
  preds
}

#' @rdname simulate_models
#'
#' @export
simulate_models.burgle_cv.glmnet <- function(object, models = NULL, newdata, type = "response", sims = 1, seed = NULL, se = FALSE, ...){
  if(is.null(models)) stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  
  # Convert newdata to matrix for prediction
  mm <- as.matrix(newdata)
  if(ncol(mm) != length(object$coef)){
    stop(paste("Number of features in newdata (", ncol(mm), ") does not match model features (", length(object$coef), ")", sep=""))
  }
  
  # Compute linear predictor: X %*% coef
  if(!is.null(dim(models))){
    preds <- fastmm(mm, t(models))
  }else{
    preds <- fastmm(mm, matrix(models))
  }
  
  if(type == "link"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'link'")
    return(preds)
  }
  
  # For response type, apply inverse link based on family
  if(type == "response"){
    # Apply inverse link
    if(is.list(preds)){
      pn <- lapply(preds, object$inv_link)
    }else{
      pn <- object$inv_link(preds)
    }
    
    family <- object$family
    if(is.character(family)) family <- tolower(family)
    
    # Simulate binomial responses if needed
    if(grepl("binomial", family)){
      if(is.list(pn)){
        pn <- lapply(pn, simulate_responses_binom, sims = 1)
      }else{
        pn <- simulate_responses_binom(pn, sims = 1)
      }
      
      pn <- lapply(pn, drop_list)
      pn <- drop_list(pn)
      return(pn)
    }
    
    return(drop_list(pn))
  }
  
  preds
}

# Utility function to simplify lists with single element
drop_list <- function(x){
  if(is.list(x) & length(x) == 1L) x <- x[[1]]
  return(x)
}
