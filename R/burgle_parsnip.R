## burgle_parsnip.R
## parsnip::model_fit - Parsnip model wrapper (parsnip package)
##
## A parsnip model_fit object wraps an underlying fit (lm, glm, coxph, etc.)
## in $fit. This thin dispatcher calls burgle() on the underlying fit, giving
## broad coverage across all parsnip-supported engines automatically.

#' @name burgle_
#'
#' @export
burgle.model_fit <- function(object, ...){
  if(is.null(object$fit)){
    stop("parsnip model_fit object has no $fit component. ",
         "Ensure the model was trained with fit() or fit_xy().")
  }
  burgle(object$fit, ...)
}
