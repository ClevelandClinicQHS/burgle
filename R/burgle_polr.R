## burgle_polr.R
## MASS::polr - Proportional odds logistic regression (MASS package)
##
## Stores the full coefficient vector (regression betas THEN threshold
## parameters zeta), the joint covariance matrix, and enough metadata to
## reconstruct the design matrix for new data.
##
## The internal parameter ordering matches the ordering used by
## MASS::vcov.polr: regression betas come first, zeta (thresholds) second.
##
## Supported link functions: logistic, probit, loglog, cloglog, cauchit.

#' @name burgle_
#'
#' @export
burgle.polr <- function(object, ...){

  beta <- stats::coef(object)    ## regression betas only
  zeta <- object$zeta            ## threshold parameters

  ## Full coefficient vector in the order that vcov.polr uses
  all_coef <- c(beta, zeta)
  cov      <- stats::vcov(object)

  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  xlevels  <- object$xlevels
  contrasts <- object$contrasts
  lev      <- object$lev       ## response category levels
  method   <- object$method    ## link name

  l <- list(coef      = all_coef,
            cov       = cov,
            terms     = terms,
            xlevels   = xlevels,
            contrasts = contrasts,
            lev       = lev,
            method    = method,
            n_beta    = length(beta),
            n_zeta    = length(zeta))

  class(l) <- "burgle_polr"
  l
}


## Internal: CDF for the link function used in polr
.polr_cdf <- function(method){
  switch(method,
    "logistic" = stats::plogis,
    "probit"   = stats::pnorm,
    "loglog"   = function(x) exp(-exp(-x)),          ## log-log (Gumbel)
    "cloglog"  = function(x) 1.0 - exp(-exp(x)),     ## complementary log-log
    "cauchit"  = stats::pcauchy,
    stop(paste("Unknown polr link:", method))
  )
}


## Internal: compute category probability matrix from eta (n x 1) and zeta
.polr_probs <- function(eta, zeta, cdf_fn){
  K   <- length(zeta) + 1L
  nObs <- length(eta)

  ## Cumulative probabilities: P(Y <= k) = F(zeta_k - eta)
  cum_probs <- matrix(NA_real_, nrow = nObs, ncol = K)
  for(k in seq_len(K - 1L)){
    cum_probs[, k] <- cdf_fn(zeta[k] - eta)
  }
  cum_probs[, K] <- 1.0

  ## Convert to category probabilities P(Y = k)
  cat_probs <- matrix(NA_real_, nrow = nObs, ncol = K)
  cat_probs[, 1L] <- cum_probs[, 1L]
  for(k in 2L:K){
    cat_probs[, k] <- cum_probs[, k] - cum_probs[, k - 1L]
  }
  ## Clip small negatives caused by floating-point
  cat_probs <- pmax(cat_probs, 0.0)
  cat_probs
}


#' @name simulate_models
#' @export
simulate_models.burgle_polr <- function(object, models = NULL, newdata,
                                        type = "probs", sims = 1L,
                                        seed = NULL, ...){

  if(is.null(models)){
    stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  }
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "probs", "response"))

  if(!is.null(seed)) set.seed(seed)

  cdf_fn  <- .polr_cdf(object$method)
  n_beta  <- object$n_beta
  n_zeta  <- object$n_zeta
  lev     <- object$lev

  ## Design matrix - polr has an intercept in the terms but uses thresholds
  ## (zeta) as intercepts, so remove the intercept column from model matrix
  mm <- stats::model.matrix(object$terms, data = newdata,
                             xlev = object$xlevels,
                             contrasts.arg = object$contrasts)
  if("(Intercept)" %in% colnames(mm)){
    mm <- mm[, -1L, drop = FALSE]
  }

  compute_for_draw <- function(coef_vec){
    beta <- coef_vec[seq_len(n_beta)]
    zeta <- coef_vec[(n_beta + 1L):(n_beta + n_zeta)]

    ## eta: linear predictor (n x 1)
    if(ncol(mm) == 0L){
      eta <- rep(0.0, nrow(newdata))
    } else {
      eta <- as.vector(fastmm(mm, matrix(beta)))
    }

    if(type == "lp") return(eta)

    probs <- .polr_probs(eta, zeta, cdf_fn)
    colnames(probs) <- lev
    if(type == "probs") return(probs)

    ## type == "response"
    replicate(sims,
              apply(probs, 1L, function(p) sample(lev, size = 1L, prob = p)),
              simplify = FALSE)
  }

  if(is.matrix(models)){
    results <- apply(models, 1L, compute_for_draw, simplify = FALSE)
    if(type == "response" && sims == 1L){
      results <- lapply(results, function(x) x[[1L]])
    }
    return(results)
  }

  result <- compute_for_draw(models)
  if(type == "response" && sims == 1L) result <- result[[1L]]
  result
}

#' @name predict_burgle
#'
#' @param type "lp" returns the linear predictor eta; "probs" returns the
#'   matrix of category probabilities (analogous to predict.polr type="probs");
#'   "response" simulates category draws.
#'
#' @export
predict.burgle_polr <- function(object, newdata, original = TRUE, draws = 1,
                                sims = 1, type = "probs", seed = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "probs", "response"))

  if(original && draws > 1L) stop("Can only have one draw from the original model")

  models <- draw_models(object, original = original, draws = draws, seed = seed)
  simulate_models(object, models = models, newdata = newdata, type = type,
                  sims = sims, seed = seed, ...)
}
