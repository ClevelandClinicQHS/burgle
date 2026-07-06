#' @name burgle_
#'
#' @export
burgle.flexsurvreg <- function(object, ...){

  # Extract regression coefficients (covariate effects only)
  coef <- stats::coef(object)
  
  # Extract formula terms from covariate structure
  terms <- object$covdata$terms
  terms <- stats::delete.response(terms)

  # Handle zero or singular covariance matrix
  if (length(coef) == 0L) {
    cov <- matrix(0)
  }
  else {
    cov <- stats::vcov(object)
    # If covariance estimation failed (e.g., from collinearity), set to 0 matrix
    # This means predictions use only point estimates without uncertainty intervals
    if(any(is.na(cov))){
      warning("No covariance estimates found, predicting will only be done from the estimated model")
      cov <- matrix(0, nrow = length(coef), ncol = length(coef))
    }
  }

  # Extract distribution functions (work for any flexsurv distribution)
  # These are dynamically determined based on the specified distribution
  pf <- object$dfns$p              # CDF function: P(T <= t)
  hz <- object$dfns$H              # Cumulative hazard function: H(t)
  qn <- object$dfns$q              # Quantile function: inverse of CDF
  
  # Unique event times from survival data
  unq <- sort(unique(object$data$Y[,"time"]))
  
  # Store factor levels and contrasts for prediction
  xlevels <- object$covdata$xlev
  contrasts <- attr(object$data$mml$mu, "contrasts")
  
  # Parameter transformation information
  # Needed to backtransform parameters after simulation
  inv_t <- object$dlist$inv.transforms
  pars_i <- object$basepars           # Distribution parameter indices (e.g., scale, shape)
  
  # Identify location parameter (typically mu or rate)
  # Location parameter gets covariate effects applied to it
  loc <- which(names(coef) == object$dlist$location)
  
  # Other parameters (shape, scale, etc.) not affected by covariates
  opars_i <- setdiff(pars_i, loc)

  # Assemble burgle object with distribution-specific information
  l <- list(coef = coef, cov = cov, xlevels = xlevels, contrasts = contrasts,
            terms = terms,
            p_f = pf, p_h = hz, p_q = qn,
            e_times = unq,
            inv.transforms = inv_t, pars_indeces = pars_i, location = loc, opars_indeces = opars_i)
  class(l) <- "burgle_flexsurvreg"
  l

}

#' @name predict_burgle
#' @importFrom stats setNames
#'
#' @export
predict.burgle_flexsurvreg <- function(object, newdata = NA, original = TRUE, draws = 1, sims = 1,
                                       type = "lp", times = NULL, ...){

  if (!is.data.frame(newdata))
    stop("newdata must be an object of class data.frame")
  
  nc <- names(object$coef)
  type <- match.arg(tolower(type), c("lp", "response", "risk", "time"))
  
  # Generate predictions for specified number of draws
  if (original & draws > 1) {
    stop("Can only have one draw from the original model")
  }
  if (original) {
    # Use point estimate (no simulation)
    models <- object$coef
  }else {
    # Simulate coefficient values from posterior distribution
    models <- MASS::mvrnorm(n = draws, mu = object$coef,
                            Sigma = object$cov)
  }

  # Parse parameters: separate location from other parameters
  if(draws == 1L){
    # Single draw: vectors of parameters
    params <- models[object$pars_indeces]
    locs <- models[object$location]
    o_params <- models[object$opars_indeces]
    
    # Extract covariate effect coefficients for later
    models <- models[-object$pars_indeces]
    
    # Back-transform non-location parameters if needed
    if(length(o_params) > 0L) {
      o_params <- mapply(function(x, y) y(x), o_params, object$inv.transforms[object$opars_indeces])
    }
    if(length(models) == 0L){
      models <- 0L
    }
  }else{
    # Multiple draws: matrices of parameters
    params <- models[,object$pars_indeces]
    locs <- models[,object$location]
    o_params <- models[,object$opars_indeces]
    
    # Extract covariate effect coefficients
    models <- matrix(models[,-object$pars_indeces], nrow= draws)
    
    # Back-transform non-location parameters
    if(is.null(dim(o_params))){
      o_params <- mapply(function(x, y) y(o_params[x]), 1:length(o_params), object$inv.transforms[object$opars_indeces])
    }else{
      o_params <- mapply(function(x, y) y(o_params[, x]), 1:ncol(o_params), object$inv.transforms[object$opars_indeces])
    }
    if(length(models) == 0L){
      models <- matrix(0, nrow = draws)
    }
  }

  # Build model matrix from newdata using stored terms and contrasts
  mm <- stats::model.matrix(object$terms, data = newdata,
                            xlev = object$xlevels, contrasts.arg = object$contrasts)[,-1]

  if(length(mm) == 0L){
    mm <- matrix(0, nrow = nrow(newdata))
  }

  if(is.vector(mm)) {mm <- matrix(mm, nrow = nrow(newdata))}

  # Calculate linear predictions: X %*% beta
  if (!is.null(dim(models))) {
    preds <- fastmm(mm, t(models))
  }else {
    preds <- as.vector(fastmm(mm, matrix(models)))
  }
  
  # Return linear predictions if requested
  if (type == "lp") {
    if (sims > 1L)
      warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  if(is.null(times) & type %in% c("response", "risk")){
    stop("times is missing")
  }

  # Add location parameter effect to linear predictions
  if(!is.null(dim(models))){
    preds <- mapply(function(x, y) preds[, x] + y, 1:length(locs), locs)
  }else{
    preds <- preds + locs
  }
  
  # Back-transform location parameter using inverse transform
  preds <- object$inv.transforms[[object$location]](preds)

  # Calculate risk predictions using hazard function
  if(draws == 1){
    # Single draw
    list_pr <- append(as.list(o_params), list(p = preds))
    names(list_pr) <- c(nc[object$opars_indeces], nc[object$location])
    
    if(type == "time"){
      # Sample from distribution at specified quantiles
      ps <- stats::runif(n = nrow(newdata))
      list_pr <- append(list_pr, list(p = ps))
      ste <- do.call(object$p_q, list_pr)
      return(ste)
    }

    # Calculate cumulative hazard at specified times
    pr0 <- sapply(times, function(y){
      list_pr_x <- append(list_pr, list(x = y))
      pr00 <- do.call(object$p_h, list_pr_x)
      pr00
    })
    
    # Convert cumulative hazard to probability
    pr0 <- 1-exp(-pr0)
    if(nrow(pr0) == 1L) pr0 <- t(pr0)
    
  }else{
    # Multiple draws
    if(length(o_params) > 0L){
      # Parameter-specific draw processing
      if(is.null(dim(o_params))){
        list_pr <- lapply(1:draws, function(x) append(as.list(o_params[x]), list(p = preds[, x])))
      }else{
        list_pr <- lapply(1:draws, function(x) append(as.list(o_params[x,]), list(p = preds[, x])))
      }
      list_pr <- lapply(list_pr, setNames, c(nc[object$opars_indeces], nc[object$location]))
    }else{
      list_pr <- lapply(1:draws, function(x) list(p = preds[, x]))
      list_pr <- lapply(list_pr, setNames, c(nc[object$location]))
    }
    
    if(type == "time"){
      # Sample times from distribution for each draw
      ps <- stats::runif(n = nrow(newdata))
      ste <- lapply(list_pr, function(x){
        list_pr_x <- append(x, list(p = ps))
        ste1 <- do.call(object$p_q, list_pr_x)
        ste1
      })
      return(ste)
    }

    # Calculate cumulative hazard for each draw
    pr0 <- lapply(list_pr, function(z){
      sapply(times, function(y){
        list_pr_x <- append(z, list(x = y))
        pr00 <- do.call(object$p_h, list_pr_x)
        pr00
      })
    })

    # Convert to probability and format output
    pr0 <- lapply(pr0, `row.names<-`, NULL)
    pr0 <- lapply(pr0, function(z) 1-exp(-z))
    if(nrow(pr0[[1]] == 1L))  pr0 <- lapply(pr0, t)
  }

  if(type == "risk"){
    return(pr0)
  }

  # Generate response predictions (with simulation if requested)
  if (sims >= 1 & type == "response") {
    if (!is.null(dim(pr0))) {
      pn <- simulate_responses_binom(pr0, sims)
      if(sims < 2) pn <- pn[[1]]
    } else {
      pn <- lapply(pr0, simulate_responses_binom, sims = sims)
      if(sims < 2) pn <- lapply(pn, function(x) if(length(x) == 1) x[[1]] else x)
    }
  }

  pn

}

flexsurv_risk <- function(f, t, start = 0, ...){
  dots <- list(...)
  r <- 1-((1 - f(t, ...))/(1 - f(start, ...)))
  r
}

## Calculate probability of survival at time t
flexsurv_ptime <- function(fp, fq, t, start = 0, ...){
  dots <- list(...)
  sp <- fp(start, ...)
  qu_f_start <- sp + (1- sp)*t
  q1 <- fq(qu_f_start, ...)
  q1
}
