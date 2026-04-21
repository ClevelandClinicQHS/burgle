#' Burgle
#'
#' Burgling what is necessary from different objects
#'
#' @name burgle_
#'
#' @param object the model object to burgle
#' @param ... must be left empty for now
#'
#' @return a burgle_ object
#' @export
#'
#' @examples
#' fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
#' bfit <- burgle(fit)
#' object.size(fit)
#' object.size(bfit)
burgle <- function(object, ...){
  UseMethod("burgle")
}

#' @rdname burgle_
#'
#' @export
burgle.lm <- function(object, ...){

  coef <- stats::coef(object)
  cov <- stats::vcov(object)
  if(any(is.na(coef))){
    warning("At least 1 coefficient has a vlue of NA")
    coef[is.na(coef)] <- 0
    cov[is.na(cov)] <- 0
  }

  mse <- sum(object$residuals ^2)/object$df.residual
  xlevels <- object$xlevels
  contrasts <- object$contrasts
  # formula <- (as.character(attr(object$terms, "predvars"))[-c(1:2)])
  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  l <- list("coef" = coef,
            "cov" = cov,
            "mse" = mse,
            "xlevels" = xlevels,
            "contrasts" = contrasts,
            # "formula" = formula,
            "terms"= terms)

  class(l) <- "burgle_lm"

  l

}

#' @rdname burgle_
#'
#' @export
burgle.glm <- function(object, ...){

  coef <- stats::coef(object)

  cov <- stats::vcov(object)

  if(any(is.na(coef))){
    warning("At least 1 coefficient has a vlue of NA")
    coef[is.na(coef)] <- 0
    cov[is.na(cov)] <- 0
  }

  mse <- sum(object$residuals ^2)/object$df.residual

  xlevels <- object$xlevels

  contrasts <- object$contrasts

  terms <- object$terms
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  family <- object$family$family

  inv_link <- object$family$linkinv

  l <- list("coef" = coef,
            "cov" = cov,
            "mse" = mse,
            "xlevels" = xlevels,
            "terms" = terms,
            "family" = family,
            "contrasts" = contrasts,
            "inv_link" = inv_link)

  class(l) <- "burgle_glm"

  l

}


