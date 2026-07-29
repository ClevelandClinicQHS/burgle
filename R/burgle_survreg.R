## burgle_survreg.R
## survival::survreg - Parametric survival regression (survival package)
##
## Stores coefficients (which include both location regression betas AND the
## log-scale parameter), their joint covariance, and enough metadata to
## reconstruct the model matrix for new data.  The burgle_survreg object is
## self-contained for predictions - the survival package is NOT required at
## prediction time.
##
## Supported distributions: weibull, exponential, gaussian, logistic,
## lognormal, loglogistic.  (The exponential distribution has a fixed scale
## parameter equal to 1, so it is handled separately.)

#' @name burgle_
#'
#' @export
burgle.survreg <- function(object, ...){

  cov  <- stats::vcov(object)

  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  xlevels  <- object$xlevels
  contrasts <- object$contrasts
  dist     <- object$dist

  ## Build full coefficient vector to align with vcov.survreg parameterization.
  ## For exponential the scale is fixed at 1 (no Log(scale) coefficient).
  coef_reg <- stats::coef(object)
  if(dist == "exponential"){
    coef_all  <- coef_reg
    loc_idx   <- seq_along(coef_reg)
    scale_idx <- NULL
  } else {
    coef_all  <- c(coef_reg, "Log(scale)" = log(object$scale))
    loc_idx   <- seq_along(coef_reg)
    scale_idx <- length(coef_all)
  }

  l <- list(coef      = coef_all,
            cov       = cov,
            terms     = terms,
            xlevels   = xlevels,
            contrasts = contrasts,
            dist      = dist,
            loc_idx   = loc_idx,
            scale_idx = scale_idx)

  class(l) <- "burgle_survreg"
  l
}


## Internal: evaluate the CDF of the survreg distribution.
.survreg_cdf <- function(t, mu, log_sig, dist){
  sigma <- exp(log_sig)
  switch(dist,
    "weibull"     = stats::pweibull(t, shape = 1.0 / sigma, scale = exp(mu)),
    "exponential" = stats::pexp(t, rate = exp(-mu)),
    "gaussian"    = stats::pnorm(t, mean = mu, sd = sigma),
    "logistic"    = stats::plogis((t - mu) / sigma),
    "lognormal"   = stats::pnorm((log(t) - mu) / sigma),
    "loglogistic" = stats::plogis((log(t) - mu) / sigma),
    stop(paste("Unsupported survreg distribution:", dist))
  )
}


#' @name predict_burgle
#'
#' @export
predict.burgle_survreg <- function(object, newdata, original = TRUE, draws = 1,
                                   sims = 1, type = "lp", times = NULL,
                                   seed = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")

  models <- draw_models(object, original = original, draws = draws, seed = seed)

  pn <- simulate_models(object, models = models, newdata = newdata, draws = draws,
                        sims = sims, type = type, times = times, seed = seed, ...)
  pn
}


#' @name simulate_models
#'
#' @export
simulate_models.burgle_survreg <- function(object, models = NULL, newdata,
                                           type = "lp", sims = 1, seed = NULL,
                                           times = NULL, ...){

  if(is.null(models)){
    stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  }
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")

  type  <- match.arg(tolower(type), c("lp", "risk", "response"))
  draws <- if(is.matrix(models)) nrow(models) else 1L

  ## Build the design matrix (full, including intercept)
  mm <- stats::model.matrix(object$terms, data = newdata,
                             xlev = object$xlevels,
                             contrasts.arg = object$contrasts)

  ## Split models into location betas and log-scale
  loc_idx   <- object$loc_idx
  scale_idx <- object$scale_idx

  if(draws == 1L){
    loc_betas <- models[loc_idx]
    log_scale <- if(!is.null(scale_idx)) models[scale_idx] else 0.0
  } else {
    loc_betas <- models[, loc_idx, drop = FALSE]
    log_scale <- if(!is.null(scale_idx)) models[, scale_idx] else rep(0.0, draws)
  }

  ## Linear predictor (location parameter mu = X * beta_location)
  if(draws == 1L){
    preds <- as.vector(fastmm(mm, matrix(loc_betas)))
  } else {
    preds <- fastmm(mm, t(loc_betas))
  }

  if(type == "lp") return(preds)

  if(is.null(times)) stop("times is missing")

  nObs <- nrow(newdata)
  n_t  <- length(times)
  dist <- object$dist

  ## Compute risk at each requested time point
  if(draws == 1L){
    pr0 <- sapply(times, function(t) .survreg_cdf(t, preds, log_scale, dist))
    if(!is.matrix(pr0)) pr0 <- matrix(pr0, nrow = nObs, ncol = n_t)
    if(n_t == 1L)       pr0 <- matrix(pr0, nrow = nObs, ncol = 1L)
  } else {
    pr0 <- lapply(seq_len(draws), function(d){
      mu_d  <- preds[, d]
      ls_d  <- log_scale[d]
      res   <- sapply(times, function(t) .survreg_cdf(t, mu_d, ls_d, dist))
      if(!is.matrix(res)) res <- matrix(res, nrow = nObs, ncol = n_t)
      if(n_t == 1L)       res <- matrix(res, nrow = nObs, ncol = 1L)
      res
    })
  }

  pr0 <- drop_list(pr0)

  if(type == "risk") return(pr0)

  ## type == "response": simulate binary event indicators
  if(is.list(pr0)){
    pn <- lapply(pr0, simulate_responses_binom, sims = sims)
    if(sims < 2L) pn <- lapply(pn, function(x) if(length(x) == 1L) x[[1L]] else x)
  } else {
    pn <- simulate_responses_binom(pr0, sims)
    if(sims < 2L) pn <- pn[[1L]]
  }

  pn
}
