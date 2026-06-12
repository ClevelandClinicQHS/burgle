#' @rdname burgle_
#'
#' @export
burgle.glmnet <- function(object, ...){
  # Extract coefficients at lambda.min (default behavior for glmnet)
  coef <- as.numeric(coef(object, s = object$lambda.min)[-1])
  
  # glmnet does not provide covariance matrix - set to zero matrix
  # This means draw_models will only return the original coefficients
  cov <- matrix(0, nrow = length(coef), ncol = length(coef))
  
  # Get family information
  family <- object$call$family
  if(is.null(family)) family <- "gaussian"
  
  # For gaussian, use MSE from model; for others, set to 0
  mse <- 0
  
  l <- list(
    "coef" = coef,
    "cov" = cov,
    "mse" = mse,
    "lambda" = object$lambda.min,
    "family" = family,
    "nfeatures" = nrow(object$beta)
  )
  
  class(l) <- "burgle_glmnet"
  
  l
}

#' @rdname burgle_
#'
#' @export
burgle.cv.glmnet <- function(object, lambda_choice = "lambda.1se", ...){
  # Get the chosen lambda
  lambda_val <- object[[lambda_choice]]
  if(is.null(lambda_val)){
    stop(paste("lambda_choice '", lambda_choice, "' not found in cv.glmnet object", sep=""))
  }
  
  # Extract coefficients at the chosen lambda
  coef <- as.numeric(coef(object, s = lambda_val)[-1])
  
  # glmnet does not provide covariance matrix - set to zero matrix
  cov <- matrix(0, nrow = length(coef), ncol = length(coef))
  
  # Get family information
  family <- object$call$family
  if(is.null(family)) family <- "gaussian"
  
  # For gaussian, use MSE from model; for others, set to 0
  mse <- 0
  
  l <- list(
    "coef" = coef,
    "cov" = cov,
    "mse" = mse,
    "lambda" = lambda_val,
    "lambda_choice" = lambda_choice,
    "family" = family,
    "nfeatures" = nrow(object$glmnet.fit$beta),
    "cv_object" = object
  )
  
  class(l) <- "burgle_cv.glmnet"
  
  l
}

# Internal function for drawing models from glmnet objects
# For glmnet, we don't have a covariance matrix, so we just return the original coefficients
draw_models_glmnet <- function(object, original = TRUE, draws = 1, seed = NULL){
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

#' @name predict_burgle
#'
#' @export
predict.burgle_glmnet <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "response", se = FALSE, seed = NULL, ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("link", "response"))
  
  if(original & draws > 1){
    stop("Can only have one draw from the original model")
  }
  
  models <- draw_models_glmnet(object, original = original, draws = draws, seed = seed)
  
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
  
  models <- draw_models_glmnet(object, original = original, draws = draws, seed = seed)
  
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
    family <- object$family
    if(is.character(family)) family <- tolower(family)
    
    if(grepl("gaussian", family)){
      pn <- preds
    }
    else if(grepl("binomial", family)){
      # Apply logistic inverse link
      inv_link <- function(x) 1 / (1 + exp(-x))
      if(is.list(preds)){
        pn <- lapply(preds, inv_link)
      }else{
        pn <- inv_link(preds)
      }
      
      # Simulate binomial responses
      if(is.list(pn)){
        pn <- lapply(pn, simulate_responses_binom, sims = 1)
      }else{
        pn <- simulate_responses_binom(pn, sims = 1)
      }
      
      pn <- lapply(pn, drop_list)
      pn <- drop_list(pn)
      return(pn)
    }
    else if(grepl("poisson", family)){
      # Apply exponential inverse link
      inv_link <- function(x) exp(x)
      if(is.list(preds)){
        pn <- lapply(preds, inv_link)
      }else{
        pn <- inv_link(preds)
      }
    }
    else if(grepl("multinomial", family)){
      # Apply softmax for multinomial
      stop("Multinomial glmnet not yet supported")
    }
    else{
      stop(paste("Family", family, "not yet supported for burgle.glmnet"))
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
    family <- object$family
    if(is.character(family)) family <- tolower(family)
    
    if(grepl("gaussian", family)){
      pn <- preds
    }
    else if(grepl("binomial", family)){
      # Apply logistic inverse link
      inv_link <- function(x) 1 / (1 + exp(-x))
      if(is.list(preds)){
        pn <- lapply(preds, inv_link)
      }else{
        pn <- inv_link(preds)
      }
      
      # Simulate binomial responses
      if(is.list(pn)){
        pn <- lapply(pn, simulate_responses_binom, sims = 1)
      }else{
        pn <- simulate_responses_binom(pn, sims = 1)
      }
      
      pn <- lapply(pn, drop_list)
      pn <- drop_list(pn)
      return(pn)
    }
    else if(grepl("poisson", family)){
      # Apply exponential inverse link
      inv_link <- function(x) exp(x)
      if(is.list(preds)){
        pn <- lapply(preds, inv_link)
      }else{
        pn <- inv_link(preds)
      }
    }
    else if(grepl("multinomial", family)){
      stop("Multinomial glmnet not yet supported")
    }
    else{
      stop(paste("Family", family, "not yet supported for burgle.cv.glmnet"))
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
