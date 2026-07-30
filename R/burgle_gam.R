## burgle_gam.R
## mgcv::gam compatibility
##
## This method supports GAM fits that use only linear predictor terms
## (no smooth basis terms). Those objects are converted to burgle_lm or
## burgle_glm so prediction follows the universal paths.

#' @name burgle_
#'
#' @export
burgle.gam <- function(object, ...){
  has_smooth <- !is.null(object$smooth) && length(object$smooth) > 0L
  if(has_smooth){
    stop("mgcv::gam models with smooth terms are not currently supported by burgle(). ",
         "Use GAM models with linear predictor terms only.")
  }
  .burgle_lm_compat(object)
}
