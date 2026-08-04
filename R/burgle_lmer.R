## burgle_lmer.R
## lme4::lmerMod / lme4::glmerMod - Linear and generalized linear mixed models
## (lme4 package)
##
## Only the FIXED-EFFECT portion of the model is burglarised.  The
## population-level linear predictor X * beta_fixed is what is captured,
## which is appropriate for microsimulation scenarios where individual-level
## random effects are either averaged out or re-drawn externally.
##
## Both lmerMod and glmerMod objects share the same internal list structure
## (burgle_merMod) via a common class hierarchy:
##   burgle_lmerMod → burgle_merMod
##   burgle_glmerMod → burgle_merMod
##
## The lme4 package is needed at burgle time (to call lme4::fixef and
## lme4::nobars). The resulting burgle object is self-contained for
## prediction (lme4 is not needed at prediction time for lmer; for glmer the
## inv_link function is stored by reference and the package is needed).

## ---- Internal helper -------------------------------------------------------

.burgle_merMod <- function(object, ...){

  ## Fixed-effect coefficients and their covariance
  coef <- as.numeric(lme4::fixef(object))
  names(coef) <- names(lme4::fixef(object))
  cov <- as.matrix(stats::vcov(object))

  ## Formula without random-effects bars: y ~ x1 + x2 (not y ~ x1 + (1|g))
  fe_formula <- lme4::nobars(stats::formula(object))
  fe_terms   <- stats::delete.response(stats::terms(fe_formula))
  attr(fe_terms, ".Environment") <- NULL

  ## Extract xlevels from the model frame (only for fixed-effect variables)
  mf       <- stats::model.frame(object)
  fe_vars  <- all.vars(fe_terms)
  fac_cols <- names(which(sapply(mf[, intersect(names(mf), fe_vars), drop = FALSE],
                                 function(x) is.factor(x) || is.ordered(x))))
  xlevels  <- if(length(fac_cols) > 0L){
    lapply(mf[, fac_cols, drop = FALSE], levels)
  } else {
    list()
  }

  ## Contrasts from a sample model matrix built from the first row of mf
  mm_sample <- stats::model.matrix(fe_terms,
                                   data = mf[1L, , drop = FALSE],
                                   xlev = xlevels)
  contrasts <- attr(mm_sample, "contrasts")

  ## GLM family / link (only for glmerMod)
  is_glmm  <- inherits(object, "glmerMod")
  family   <- if(is_glmm) object@resp$family$family   else "gaussian"
  inv_link <- if(is_glmm) object@resp$family$linkinv  else identity

  l <- list(coef      = coef,
            cov       = cov,
            terms     = fe_terms,
            xlevels   = xlevels,
            contrasts = contrasts,
            family    = family,
            inv_link  = inv_link)

  sub_class <- if(is_glmm) "burgle_glmerMod" else "burgle_lmerMod"
  class(l) <- c(sub_class, "burgle_merMod")
  l
}


## ---- S3 burgle methods -----------------------------------------------------

#' @name burgle_
#' @export
burgle.lmerMod <- function(object, ...) .burgle_merMod(object, ...)

#' @name burgle_
#' @export
burgle.glmerMod <- function(object, ...) .burgle_merMod(object, ...)


## ---- Simulate helper -------------------------------------------------------

#' @name simulate_models
#' @export
simulate_models.burgle_merMod <- function(object, models = NULL, newdata,
                                          type = "lp", sims = 1L, seed = NULL,
                                          se = FALSE, ...){

  if(is.null(models)){
    stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  }
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")

  is_glmm <- inherits(object, "burgle_glmerMod")

  valid_types <- if(is_glmm) c("lp", "link", "response") else c("lp", "response")
  type <- match.arg(tolower(type), valid_types)

  mm <- stats::model.matrix(object$terms, data = newdata,
                             xlev = object$xlevels,
                             contrasts.arg = object$contrasts)

  if(!is.null(dim(models))){
    preds <- fastmm(mm, t(models))
  } else {
    preds <- fastmm(mm, matrix(models))
  }

  if(type == "lp"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  ## For glmm: apply inverse link and optionally simulate binary responses
  if(is_glmm){
    ## se-based simulation is not supported for merMod (no residual MSE)
    preds_link <- if(is.matrix(preds)){
      apply(preds, 2L, object$inv_link)
    } else {
      object$inv_link(preds)
    }

    if(type == "link") return(preds_link)

    ## type == "response" - binomial simulation
    if(!grepl("binomial", object$family)){
      stop("type = 'response' is only supported for binomial glmer models; ",
           "use type = 'link' for other families")
    }
    preds_m <- if(is.matrix(preds_link)) preds_link else matrix(preds_link)
    pn <- simulate_responses_binom(preds_m, sims)
    pn <- lapply(pn, drop_list)
    pn <- drop_list(pn)
    return(pn)
  }

  ## For lmm: simulate from Normal using confidence-interval SE.
  ## Note: lmm has no separate residual MSE so only confidence-interval SE
  ## (from the fixed-effect vcov) is available.
  if(!is.matrix(preds)) preds <- matrix(preds)
  se_p <- sqrt(rowSums(fastmm(mm, object$cov) * mm))

  pn <- simulate_responses(preds, sims, se, se_p)
  pn <- drop_list(pn)
  pn
}


## ---- Predict methods -------------------------------------------------------

#' @name predict_burgle
#' @export
predict.burgle_lmerMod <- function(object, newdata, original = TRUE, draws = 1L,
                                   sims = 1L, type = "lp", se = FALSE,
                                   seed = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  if(original && draws > 1L) stop("Can only have one draw from the original model")

  models <- draw_models(object, original = original, draws = draws, seed = seed)

  simulate_models(object, models = models, newdata = newdata, sims = sims,
                  type = type, se = se, seed = seed, ...)
}

#' @name predict_burgle
#' @export
predict.burgle_glmerMod <- function(object, newdata, original = TRUE, draws = 1L,
                                    sims = 1L, type = "lp", seed = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  if(original && draws > 1L) stop("Can only have one draw from the original model")

  models <- draw_models(object, original = original, draws = draws, seed = seed)

  simulate_models(object, models = models, newdata = newdata, sims = sims,
                  type = type, seed = seed, ...)
}
