## burgle_betareg.R
## betareg::betareg - Beta regression (betareg package)
##
## Beta regression has two sub-models: a mean model and a precision (phi)
## model.  Both are stored together in the burgle object.
##
## The full coefficient vector follows the ordering used by coef.betareg
## (and vcov.betareg): mean-model coefficients first, then precision-model
## coefficients.
##
## Prediction types:
##   "lp"       - linear predictor for the mean model (before link inversion)
##   "link"     - expected mean (inv_link applied to linear predictor)
##   "response" - simulated values from Beta(mu * phi, (1 - mu) * phi)

#' @name burgle_
#'
#' @export
burgle.betareg <- function(object, ...){

  ## Full coef and joint vcov (mean then precision, as in coef.betareg)
  coef <- stats::coef(object)
  cov  <- stats::vcov(object)

  ## Identify indices for mean and precision components
  ## coef names are like "mean_(Intercept)", "mean_x1", "precision_(Intercept)"
  n_mean <- length(object$coefficients$mean)
  n_prec <- length(object$coefficients$precision)
  mean_idx <- seq_len(n_mean)
  prec_idx <- n_mean + seq_len(n_prec)

  ## Terms for each sub-model
  mean_terms <- object$terms$mean
  mean_terms <- stats::delete.response(mean_terms)
  attr(mean_terms, ".Environment") <- NULL

  prec_terms <- object$terms$precision
  prec_terms <- stats::delete.response(prec_terms)
  attr(prec_terms, ".Environment") <- NULL

  ## Factor levels (betareg stores $levels)
  xlevels  <- object$levels
  contrasts <- object$contrasts

  ## Link functions (stored as function objects to avoid package dependency)
  inv_link_mean <- object$link$mean$linkinv
  inv_link_prec <- object$link$precision$linkinv

  l <- list(coef           = coef,
            cov            = cov,
            mean_terms     = mean_terms,
            prec_terms     = prec_terms,
            xlevels        = xlevels,
            contrasts      = contrasts,
            inv_link_mean  = inv_link_mean,
            inv_link_prec  = inv_link_prec,
            mean_idx       = mean_idx,
            prec_idx       = prec_idx)

  class(l) <- "burgle_betareg"
  l
}


#' @name simulate_models
#' @export
simulate_models.burgle_betareg <- function(object, models = NULL, newdata,
                                           type = "lp", sims = 1L,
                                           seed = NULL, ...){

  if(is.null(models)){
    stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  }
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "link", "response"))

  if(!is.null(seed)) set.seed(seed)

  ## Design matrices for mean and precision sub-models
  mm_mean <- stats::model.matrix(object$mean_terms, data = newdata,
                                  xlev = object$xlevels,
                                  contrasts.arg = object$contrasts)
  mm_prec <- stats::model.matrix(object$prec_terms, data = newdata,
                                  xlev = object$xlevels,
                                  contrasts.arg = object$contrasts)

  nObs     <- nrow(newdata)
  mean_idx <- object$mean_idx
  prec_idx <- object$prec_idx

  compute_for_draw <- function(coef_vec){
    beta_mean <- coef_vec[mean_idx]
    beta_prec <- coef_vec[prec_idx]

    lp_mean <- as.vector(fastmm(mm_mean, matrix(beta_mean)))
    lp_prec <- as.vector(fastmm(mm_prec, matrix(beta_prec)))

    if(type == "lp") return(lp_mean)

    mu  <- object$inv_link_mean(lp_mean)
    phi <- object$inv_link_prec(lp_prec)

    if(type == "link") return(mu)

    ## type == "response": simulate from Beta distribution
    ## Beta(shape1 = mu * phi, shape2 = (1 - mu) * phi)
    replicate(sims,
              stats::rbeta(nObs, shape1 = mu * phi, shape2 = (1.0 - mu) * phi),
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
#' @param type "lp" returns the linear predictor for the mean model;
#'   "link" returns the expected mean (mu) after applying the mean-model
#'   inverse link function; "response" simulates values from the beta
#'   distribution using mu and the precision parameter phi.
#'
#' @export
predict.burgle_betareg <- function(object, newdata, original = TRUE, draws = 1L,
                                   sims = 1L, type = "lp", seed = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "link", "response"))
  if(original && draws > 1L) stop("Can only have one draw from the original model")

  models <- draw_models(object, original = original, draws = draws, seed = seed)
  simulate_models(object, models = models, newdata = newdata, type = type,
                  sims = sims, seed = seed, ...)
}
