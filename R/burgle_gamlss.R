## burgle_gamlss.R
## gamlss::gamlss - Generalized Additive Models for Location, Scale, and Shape
##
## gamlss models can have up to four distribution parameter sub-models:
##   mu    - location
##   sigma - scale
##   nu    - shape 1
##   tau   - shape 2
##
## Each active sub-model has its own coefficient vector, covariance matrix,
## terms object, and inverse link function.  burgle extracts all active
## sub-models and stores just what is needed for simulate / predict.
##
## Prediction types:
##   "lp"       - linear predictor for the mu sub-model (before link inversion)
##   "link"     - expected mu after applying the mu inverse link function
##   "response" - simulated values drawn from the fitted distribution family
##
## NOTE: "response" simulation uses the gamlss family's r-function (e.g.
##   rNO, rGA, rBE …) which must be available at prediction time (the gamlss
##   package must be installed).  "lp" and "link" work without gamlss at
##   prediction time.

#' @name burgle_
#'
#' @export
burgle.gamlss <- function(object, ...){

  ## ---- helper: extract one sub-model ----------------------------------------
  .extract_submodel <- function(par){
    coef_slot  <- paste0(par, ".coefficients")
    vcov_slot  <- paste0(par, ".vcov")
    terms_slot <- paste0(par, ".terms")
    link_slot  <- paste0(par, ".link")

    coef_vec <- object[[coef_slot]]
    if(is.null(coef_vec)) return(NULL)

    cov_mat <- object[[vcov_slot]]
    ## gamlss sometimes omits the vcov for sigma/nu/tau when it is a fixed
    ## intercept-only model; fall back to a zero matrix so draw_models still
    ## works correctly in those cases.
    if(is.null(cov_mat)){
      n <- length(coef_vec)
      cov_mat <- matrix(0.0, nrow = n, ncol = n,
                        dimnames = list(names(coef_vec), names(coef_vec)))
    }

    terms_obj <- object[[terms_slot]]
    if(!is.null(terms_obj)){
      terms_obj <- stats::delete.response(terms_obj)
      attr(terms_obj, ".Environment") <- NULL
    }

    link_name <- object[[link_slot]]
    ## Resolve the inverse link as a function, keeping it self-contained
    inv_link <- .gamlss_inv_link(link_name)

    list(coef     = coef_vec,
         cov      = cov_mat,
         terms    = terms_obj,
         inv_link = inv_link,
         link     = link_name)
  }
  ## ---------------------------------------------------------------------------

  ## Extract all four potential sub-models
  pars <- c("mu", "sigma", "nu", "tau")
  sub  <- lapply(stats::setNames(pars, pars), .extract_submodel)
  ## Drop parameters not present in this model
  active_pars <- Filter(Negate(is.null), sub)

  ## Build the joint coefficient vector in the canonical ordering
  ## mu | sigma | nu | tau (only active parameters)
  all_coef <- unlist(lapply(active_pars, `[[`, "coef"), use.names = TRUE)

  ## Joint covariance: block-diagonal from per-parameter covariance matrices
  ## (gamlss estimates each parameter equation separately)
  cov_blocks <- lapply(active_pars, `[[`, "cov")
  joint_cov  <- .block_diag(cov_blocks)
  rownames(joint_cov) <- colnames(joint_cov) <- names(all_coef)

  ## Index ranges for each parameter within all_coef
  idx_list <- .make_idx_list(active_pars)

  ## Factor levels and contrasts for model.matrix reconstruction
  xlevels   <- object$xlevels
  contrasts <- object$contrasts

  ## Family name (used to dispatch the r-function for response simulation)
  family_name <- if(is.character(object$family)) object$family[[1L]] else
                   class(object$family)[[1L]]

  ## Return only active sub-model metadata (terms, inv_link, link name)
  active_meta <- lapply(active_pars, function(s)
    list(terms    = s$terms,
         inv_link = s$inv_link,
         link     = s$link))

  l <- list(coef        = all_coef,
            cov         = joint_cov,
            active_pars = names(active_pars),
            idx         = idx_list,
            meta        = active_meta,
            xlevels     = xlevels,
            contrasts   = contrasts,
            family      = family_name)

  class(l) <- "burgle_gamlss"
  l
}


## ---- helpers ----------------------------------------------------------------

## Resolve a gamlss link name to an inverse-link function
.gamlss_inv_link <- function(link){
  if(is.null(link) || !nzchar(link)) return(identity)
  switch(link,
    "identity" = identity,
    "log"      = exp,
    "logit"    = stats::plogis,
    "probit"   = stats::pnorm,
    "cloglog"  = function(x) 1 - exp(-exp(x)),
    "sqrt"     = function(x) x^2,
    "inverse"  = function(x) 1 / x,
    ## fallback: attempt make.link
    tryCatch(stats::make.link(link)$linkinv, error = function(e) identity)
  )
}

## Build a block-diagonal matrix from a list of matrices
.block_diag <- function(mlist){
  if(length(mlist) == 0L) return(matrix(0.0, 0L, 0L))
  dims <- sapply(mlist, nrow)
  n    <- sum(dims)
  out  <- matrix(0.0, nrow = n, ncol = n)
  pos  <- 0L
  for(m in mlist){
    d <- nrow(m)
    out[(pos + 1L):(pos + d), (pos + 1L):(pos + d)] <- m
    pos <- pos + d
  }
  out
}

## Build named list of integer index vectors for each parameter
.make_idx_list <- function(active_pars){
  idx  <- list()
  pos  <- 0L
  for(nm in names(active_pars)){
    len      <- length(active_pars[[nm]]$coef)
    idx[[nm]] <- pos + seq_len(len)
    pos      <- pos + len
  }
  idx
}

## Build design matrix for one sub-model parameter
.gamlss_mm <- function(meta_par, newdata, xlevels, contrasts){
  terms_obj <- meta_par$terms
  if(is.null(terms_obj)){
    ## intercept-only model: return column of ones
    return(matrix(1.0, nrow = nrow(newdata), ncol = 1L,
                  dimnames = list(NULL, "(Intercept)")))
  }
  stats::model.matrix(terms_obj, data = newdata,
                      xlev        = xlevels,
                      contrasts.arg = contrasts)
}


## ---- simulate_models --------------------------------------------------------

#' @name simulate_models
#' @export
simulate_models.burgle_gamlss <- function(object, models = NULL, newdata,
                                          type  = "lp",
                                          sims  = 1L,
                                          seed  = NULL, ...){

  if(is.null(models)){
    stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  }
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "link", "response"))

  if(!is.null(seed)) set.seed(seed)

  ## Pre-build design matrices for each active sub-model parameter
  mm_list <- lapply(object$meta, .gamlss_mm,
                    newdata   = newdata,
                    xlevels   = object$xlevels,
                    contrasts = object$contrasts)

  nObs  <- nrow(newdata)
  idx   <- object$idx
  pars  <- object$active_pars

  compute_for_draw <- function(coef_vec){
    ## Linear predictors for each parameter
    lp_vals <- lapply(pars, function(p){
      as.vector(fastmm(mm_list[[p]], matrix(coef_vec[idx[[p]]])))
    })
    names(lp_vals) <- pars

    if(type == "lp") return(lp_vals[["mu"]])

    ## Apply inverse links to get distribution parameters
    par_vals <- lapply(pars, function(p){
      object$meta[[p]]$inv_link(lp_vals[[p]])
    })
    names(par_vals) <- pars

    if(type == "link") return(par_vals[["mu"]])

    ## type == "response": simulate from the fitted distribution
    rfun <- .gamlss_rfun(object$family)

    replicate(sims, {
      args <- c(list(n = nObs), par_vals)
      do.call(rfun, args)
    }, simplify = FALSE)
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


## Resolve the random-variate function for a gamlss family
.gamlss_rfun <- function(family_name){
  ## gamlss family r-functions are named r<FAMILY>, e.g. rNO, rGA, rBE
  rfun_name <- paste0("r", family_name)
  rfun <- tryCatch(
    get(rfun_name, envir = asNamespace("gamlss.dist"), inherits = FALSE),
    error = function(e) NULL
  )
  if(is.null(rfun)){
    rfun <- tryCatch(
      get(rfun_name, envir = .GlobalEnv, inherits = TRUE),
      error = function(e) NULL
    )
  }
  if(is.null(rfun)){
    stop("Cannot find random variate function '", rfun_name,
         "' for gamlss family '", family_name, "'. ",
         "Ensure the gamlss or gamlss.dist package is installed and loaded.")
  }
  rfun
}


## ---- predict ----------------------------------------------------------------

#' @name predict_burgle
#'
#' @param type "lp" returns the linear predictor for the mu sub-model;
#'   "link" returns the expected mu after applying the mu inverse link
#'   function; "response" simulates values from the fitted gamlss distribution
#'   (requires gamlss or gamlss.dist to be installed).
#'
#' @export
predict.burgle_gamlss <- function(object, newdata,
                                  original = TRUE,
                                  draws    = 1L,
                                  sims     = 1L,
                                  type     = "lp",
                                  seed     = NULL, ...){

  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "link", "response"))

  if(original && draws > 1L) stop("Can only have one draw from the original model")

  models <- draw_models(object, original = original, draws = draws, seed = seed)
  simulate_models(object, models = models, newdata = newdata,
                  type = type, sims = sims, seed = seed, ...)
}
