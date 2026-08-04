## burgle_lm_compat.R
## Compatibility methods for fast/large linear-model fit objects so they
## produce lm/glm-compatible burgle objects and can reuse universal predict().

.burgle_mse <- function(object){
  if(is.numeric(object$mse) && length(object$mse) == 1L && is.finite(object$mse)){
    return(as.numeric(object$mse))
  }
  if(is.numeric(object$sigma) && length(object$sigma) == 1L && is.finite(object$sigma)){
    return(as.numeric(object$sigma)^2)
  }
  if(is.numeric(object$dispersion) && length(object$dispersion) == 1L && is.finite(object$dispersion)){
    return(as.numeric(object$dispersion))
  }

  df_resid <- object$df.residual
  if(is.null(df_resid)) df_resid <- object$df.resid
  if(is.null(df_resid)) df_resid <- object$df_resid

  if(is.numeric(object$deviance) && length(object$deviance) == 1L &&
     is.finite(object$deviance) && is.numeric(df_resid) && length(df_resid) == 1L &&
     is.finite(df_resid) && df_resid > 0){
    return(as.numeric(object$deviance) / as.numeric(df_resid))
  }

  resid <- object$residuals
  if(is.numeric(resid) && length(resid) > 0L &&
     is.numeric(df_resid) && length(df_resid) == 1L &&
     is.finite(df_resid) && df_resid > 0){
    return(sum(resid^2) / as.numeric(df_resid))
  }

  0
}

.burgle_terms_from_coef <- function(coef_names){
  if(is.null(coef_names) || length(coef_names) == 0L){
    stop("Could not recover model terms for this object")
  }
  nn <- coef_names[!is.na(coef_names)]
  nn <- nn[nn != "(Intercept)"]
  if(length(nn) == 0L){
    stats::terms(stats::as.formula("~ 1"))
  } else {
    stats::terms(stats::as.formula(
      paste("~", paste(sprintf("`%s`", nn), collapse = " + "))
    ))
  }
}

.burgle_terms <- function(object, coef_names){
  terms <- object$terms
  if(is.null(terms)){
    terms <- tryCatch(stats::terms(object), error = function(e) NULL)
  }
  if(is.null(terms)){
    fm <- tryCatch(stats::formula(object), error = function(e) NULL)
    if(!is.null(fm)){
      terms <- stats::terms(fm)
    }
  }
  if(is.null(terms)){
    terms <- .burgle_terms_from_coef(coef_names)
  }
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL
  terms
}

.burgle_xlevels <- function(object, terms){
  if(is.list(object$xlevels)) return(object$xlevels)

  tx <- attr(terms, "xlevels")
  if(is.list(tx)) return(tx)

  mf <- tryCatch(stats::model.frame(object), error = function(e) NULL)
  if(is.data.frame(mf)){
    vars <- all.vars(terms)
    vars <- intersect(vars, names(mf))
    if(length(vars) > 0L){
      fac_cols <- vars[sapply(mf[, vars, drop = FALSE], function(x) is.factor(x) || is.ordered(x))]
      if(length(fac_cols) > 0L){
        return(lapply(mf[, fac_cols, drop = FALSE], levels))
      }
    }
  }

  list()
}

.burgle_contrasts <- function(object, terms, xlevels){
  if(!is.null(object$contrasts)) return(object$contrasts)

  mf <- tryCatch(stats::model.frame(object), error = function(e) NULL)
  if(is.data.frame(mf) && nrow(mf) > 0L){
    mm <- tryCatch(
      stats::model.matrix(terms, data = mf[1L, , drop = FALSE], xlev = xlevels),
      error = function(e) NULL
    )
    if(!is.null(mm)){
      return(attr(mm, "contrasts"))
    }
  }
  NULL
}

.burgle_lm_compat <- function(object){
  coef <- stats::coef(object)
  cov  <- stats::vcov(object)
  terms <- .burgle_terms(object, names(coef))
  xlevels <- .burgle_xlevels(object, terms)
  contrasts <- .burgle_contrasts(object, terms, xlevels)
  mse <- .burgle_mse(object)

  family_obj <- object$family
  family_name <- if(is.list(family_obj)) family_obj$family else NULL
  is_gaussian <- is.null(family_name) || family_name %in% c("gaussian", "Gaussian")

  if(is_gaussian){
    l <- list(coef = coef,
              cov = cov,
              mse = mse,
              xlevels = xlevels,
              contrasts = contrasts,
              terms = terms)
    class(l) <- "burgle_lm"
    return(l)
  }

  inv_link <- if(is.list(family_obj) && is.function(family_obj$linkinv)) family_obj$linkinv else identity
  l <- list(coef = coef,
            cov = cov,
            mse = mse,
            xlevels = xlevels,
            contrasts = contrasts,
            terms = terms,
            family = family_name,
            inv_link = inv_link)
  class(l) <- "burgle_glm"
  l
}

#' @name burgle_
#'
#' @export
burgle.biglm <- function(object, ...){
  .burgle_lm_compat(object)
}

#' @name burgle_
#'
#' @export
burgle.bigglm <- function(object, ...){
  .burgle_lm_compat(object)
}

#' @name burgle_
#'
#' @export
burgle.speedlm <- function(object, ...){
  .burgle_lm_compat(object)
}

#' @name burgle_
#'
#' @export
burgle.speedglm <- function(object, ...){
  .burgle_lm_compat(object)
}

#' @name burgle_
#'
#' @export
burgle.fastLm <- function(object, ...){
  .burgle_lm_compat(object)
}
