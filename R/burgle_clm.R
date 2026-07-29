## burgle_clm.R
## ordinal::clm - Cumulative link model (ordinal package)
##
## The clm coefficient vector returned by coef() has threshold parameters
## (alpha) FIRST, followed by regression betas.  This matches the ordering
## used by vcov.clm.
##
## Prediction types:
##   "lp"       - linear predictor eta = X * beta (one value per observation)
##   "probs"    - matrix of category probabilities (K columns)
##   "response" - simulated category draws
##
## Supported link functions: logit, probit, loglog, cloglog, cauchit.

#' @name burgle_
#'
#' @export
burgle.clm <- function(object, ...){

  ## coef() returns c(alpha, beta) for clm
  all_coef <- stats::coef(object)
  cov      <- stats::vcov(object)

  n_alpha <- length(object$alpha)
  n_beta  <- length(object$beta)

  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  ## clm stores factor levels in $xlevels
  xlevels  <- object$xlevels
  contrasts <- object$contrasts
  y_levels <- object$y.levels    ## response category labels
  link     <- object$link        ## link name

  l <- list(coef      = all_coef,
            cov       = cov,
            terms     = terms,
            xlevels   = xlevels,
            contrasts = contrasts,
            y_levels  = y_levels,
            link      = link,
            n_alpha   = n_alpha,
            n_beta    = n_beta)

  class(l) <- "burgle_clm"
  l
}


## Internal: CDF for clm link functions
.clm_cdf <- function(link){
  switch(link,
    "logit"   = stats::plogis,
    "probit"  = stats::pnorm,
    "loglog"  = function(x) exp(-exp(-x)),
    "cloglog" = function(x) 1.0 - exp(-exp(x)),
    "cauchit" = stats::pcauchy,
    stop(paste("Unknown clm link:", link))
  )
}


#' @name predict_burgle
#'
#' @param type "lp" returns the linear predictor eta; "probs" returns the
#'   matrix of category probabilities; "response" simulates category draws.
#'
#' @name simulate_models
#' @export
simulate_models.burgle_clm <- function(object, models = NULL, newdata,
                                       type = "probs", sims = 1L,
                                       seed = NULL, ...){

  if(is.null(models)){
    stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  }
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "probs", "response"))

  if(!is.null(seed)) set.seed(seed)

  cdf_fn   <- .clm_cdf(object$link)
  n_alpha  <- object$n_alpha
  n_beta   <- object$n_beta
  y_levels <- object$y_levels
  K        <- length(y_levels)

  ## Build model matrix - clm does not use the intercept from model.matrix
  ## (threshold parameters alpha serve as intercepts)
  mm <- stats::model.matrix(object$terms, data = newdata,
                             xlev = object$xlevels,
                             contrasts.arg = object$contrasts)
  if("(Intercept)" %in% colnames(mm)){
    mm <- mm[, -1L, drop = FALSE]
  }

  compute_for_draw <- function(coef_vec){
    alpha <- coef_vec[seq_len(n_alpha)]
    beta  <- if(n_beta > 0L) coef_vec[(n_alpha + 1L):(n_alpha + n_beta)] else numeric(0L)

    ## eta: linear predictor
    if(n_beta == 0L || ncol(mm) == 0L){
      eta <- rep(0.0, nrow(newdata))
    } else {
      eta <- as.vector(fastmm(mm, matrix(beta)))
    }

    if(type == "lp") return(eta)

    ## Cumulative probabilities: P(Y <= k) = F(alpha_k - eta)
    nObs <- length(eta)
    cum_probs <- matrix(NA_real_, nrow = nObs, ncol = K)
    for(k in seq_len(K - 1L)){
      cum_probs[, k] <- cdf_fn(alpha[k] - eta)
    }
    cum_probs[, K] <- 1.0

    ## Category probabilities
    cat_probs <- matrix(NA_real_, nrow = nObs, ncol = K)
    cat_probs[, 1L] <- cum_probs[, 1L]
    for(k in 2L:K){
      cat_probs[, k] <- cum_probs[, k] - cum_probs[, k - 1L]
    }
    cat_probs <- pmax(cat_probs, 0.0)
    colnames(cat_probs) <- y_levels

    if(type == "probs") return(cat_probs)

    ## type == "response"
    replicate(sims,
              apply(cat_probs, 1L,
                    function(p) sample(y_levels, size = 1L, prob = p)),
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
#' @export
predict.burgle_clm <- function(object, newdata, original = TRUE, draws = 1L,
                               sims = 1L, type = "probs", seed = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "probs", "response"))
  if(original && draws > 1L) stop("Can only have one draw from the original model")

  models <- draw_models(object, original = original, draws = draws, seed = seed)
  simulate_models(object, models = models, newdata = newdata, type = type,
                  sims = sims, seed = seed, ...)
}
