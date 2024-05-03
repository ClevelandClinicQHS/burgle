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
  rss <- sum(object$residuals ^2)/object$df.residual
  xlevels <- object$xlevels
  # formula <- stats::reformulate(as.character(attr(object$terms, "predvars"))[-c(1:2)])
  formula <- (as.character(attr(object$terms, "predvars"))[-c(1:2)])

  l <- list("coef" = coef,
            "cov" = cov,
            "rss" = rss,
            "xlevels" = xlevels,
            "formula" = formula)

  class(l) <- "burgle_lm"

  l

}

#' @rdname burgle_
#'
#' @export
burgle.glm <- function(object, ...){

  coef <- stats::coef(object)

  cov <- stats::vcov(object)

  rss <- sum(object$residuals ^2)/object$df.residual

  xlevels <- object$xlevels

  # formula <- stats::reformulate(as.character(attr(object$terms, "predvars"))[-c(1:2)])
  formula <- (as.character(attr(object$terms, "predvars"))[-c(1:2)])

  family <- object$family$family

  inv_link <- object$family$linkinv

  l <- list("coef" = coef,
            "cov" = cov,
            "rss" = rss,
            "xlevels" = xlevels,
            "formula" = formula,
            "family" = family,
            "inv_link" = inv_link)

  class(l) <- "burgle_glm"

  l

}


