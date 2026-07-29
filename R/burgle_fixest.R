## burgle_fixest.R
## fixest::feols / fixest::feglm - Fixed-effects regression (fixest package)
##
## Only the STRUCTURAL (non-fixed-effect) coefficients are burglarised.
## These are the coefficients returned by coef(feols_fit), which exclude the
## absorbed individual fixed effects.  This is appropriate for population-
## level linear predictions on new data.
##
## The fixest package is required at burgle time (to extract the structural
## formula).  Prediction requires only the stored terms/xlevels/contrasts and
## does NOT require fixest to be installed.

#' @name burgle_
#'
#' @export
burgle.fixest <- function(object, ...){

  if(!requireNamespace("fixest", quietly = TRUE)){
    stop("The 'fixest' package must be installed to use burgle() on fixest objects")
  }

  coef <- stats::coef(object)
  cov  <- stats::vcov(object)

  ## Extract the structural (linear) formula - the part before | in feols
  structural_formula <- fixest::formula(object, type = "linear")
  structural_terms   <- stats::delete.response(stats::terms(structural_formula))
  attr(structural_terms, ".Environment") <- NULL

  ## xlevels from the model frame (only for structural variables)
  mf      <- stats::model.frame(object)
  fe_vars <- all.vars(structural_terms)
  fac_cols <- names(which(sapply(
    mf[, intersect(names(mf), fe_vars), drop = FALSE],
    function(x) is.factor(x) || is.ordered(x)
  )))
  xlevels <- if(length(fac_cols) > 0L){
    lapply(mf[, fac_cols, drop = FALSE], levels)
  } else {
    list()
  }

  ## Contrasts from a sample model matrix
  mm_sample <- stats::model.matrix(structural_terms,
                                   data = mf[1L, , drop = FALSE],
                                   xlev = xlevels)
  contrasts <- attr(mm_sample, "contrasts")

  ## Family info (feglm has a non-Gaussian family; feols is always gaussian)
  if(is.null(object$family)){
    family_name <- "gaussian"
    inv_link    <- identity
  } else {
    family_name <- object$family$family
    inv_link    <- object$family$linkinv
  }
  l <- list(coef      = coef,
            cov       = cov,
            terms     = structural_terms,
            xlevels   = xlevels,
            contrasts = contrasts,
            family    = family_name,
            inv_link  = inv_link)

  class(l) <- "burgle_fixest"
  l
}


#' @name simulate_models
#' @export
simulate_models.burgle_fixest <- function(object, models = NULL, newdata,
                                          type = "lp", sims = 1L, seed = NULL,
                                          se = FALSE, ...){

  if(is.null(models)){
    stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  }
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")

  is_glm     <- !(object$family %in% c("gaussian", "Gaussian"))
  valid_types <- if(is_glm) c("lp", "link", "response") else c("lp", "response")
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

  if(!is_glm){
    ## OLS-style feols: simulate Normal responses
    ## Fixed-effect models do not carry a residual MSE by default; use 0 for se
    se_p <- rowSums(fastmm(mm, object$cov) * mm)
    if(!is.matrix(preds)) preds <- matrix(preds)
    pn <- simulate_responses(preds, sims, se, sqrt(se_p))
    pn <- drop_list(pn)
    return(pn)
  }

  ## GLM-style feglm: apply inverse link
  preds_link <- if(is.matrix(preds)){
    apply(preds, 2L, object$inv_link)
  } else {
    object$inv_link(preds)
  }

  if(type == "link") return(preds_link)

  ## type == "response" - binomial only
  if(!grepl("binomial", object$family)){
    stop("type = 'response' is only supported for binomial feglm models; ",
         "use type = 'link' for other families")
  }
  preds_m <- if(is.matrix(preds_link)) preds_link else matrix(preds_link)
  pn <- simulate_responses_binom(preds_m, sims)
  pn <- lapply(pn, drop_list)
  pn <- drop_list(pn)
  pn
}


#' @name predict_burgle
#' @export
predict.burgle_fixest <- function(object, newdata, original = TRUE, draws = 1L,
                                  sims = 1L, type = "lp", se = FALSE,
                                  seed = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  if(original && draws > 1L) stop("Can only have one draw from the original model")

  models <- draw_models(object, original = original, draws = draws, seed = seed)

  simulate_models(object, models = models, newdata = newdata, sims = sims,
                  type = type, se = se, seed = seed, ...)
}
