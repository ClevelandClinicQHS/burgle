#' @name burgle_
#'
#' @export
burgle.cph <- function(object, ...){
  
  # Extract baseline hazard function
  # centered = FALSE: returns survival time-specific baseline hazards
  bh <- survival::basehaz(object, centered = FALSE)
  
  # Prepare terms object for prediction
  # delete.response removes the Surv() from formula for later model.matrix calls
  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  # STRATIFIED COX MODELS: Handle multiple baseline hazards
  # In stratified models, basehaz() returns one row per unique (hazard, strata) combination
  # We must remove true duplicates while preserving strata-specific hazards
  has_strata <- !is.null(object$strata) || "strata" %in% colnames(bh)
  if (has_strata) {
    # Create composite key of (hazard, strata) to identify true duplicates
    # Different strata with same hazard value should NOT be considered duplicates
    bh0 <- bh[, c("hazard", "strata")]
    bh <- bh[!duplicated(bh0), ]
    
    # Strip strata special term from formula for model matrix construction
    # Strata affects stratified baseline hazards but not covariate coefficients
    terms <- strip_strata_terms(terms)

  } else {
    # NON-STRATIFIED: Remove only true duplicate hazard entries
    # (rare but can occur with tied survival times)
    bh <- bh[!duplicated(bh$hazard), ]
  }
  
  # Extract regression coefficients and covariance
  coef <- stats::coef(object)
  if (length(coef) == 0L) {
    # Null model case: no covariates
    cov <- matrix(0)
  }
  else {
    cov <- stats::vcov(object)
  }
  
  # Calculate residual sum of squares
  # Note: Uses sum(object$n) as denominator (total number of subjects)
  rss <- sum(object$residuals^2)/(sum(object$n) - length(coef))
  
  # Store factor information for prediction stage
  xlevels <- object$xlevels
  contrasts <- object$contrasts

  # Assemble burgle_cph object
  l <- list(coef = coef, cov = cov, rss = rss, xlevels = xlevels,
            terms = terms,
            contrasts = contrasts,
            basehaz = bh)
  class(l) <- "burgle_cph"
  l
}

#' @name predict_burgle
#'
#' @export
predict.burgle_cph <- function(object, ...){
  preds <- predict.burgle_coxph(object, ...)
  preds
}

#' @name predict_burgle
#'
#' @export
simulate_models.burgle_cph <- function(object,  ...){
  sims <- simulate_models.burgle_coxph(object, ...)
  sims
}
