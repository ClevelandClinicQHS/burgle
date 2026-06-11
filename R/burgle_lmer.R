#' @rdname burgle_
#'
#' @export
burgle.lmerMod <- function(object, ...){
  coef <- lme4::fixef(object)
  cov <- as.matrix(stats::vcov(object))

  if(any(is.na(coef))){
    warning("At least 1 coefficient has a value of NA")
    coef[is.na(coef)] <- 0
    cov[is.na(cov)] <- 0
  }

  ff <- lme4::nobars(stats::formula(object))
  terms <- stats::terms(ff)
  terms <- stats::delete.response(terms)
  attr(terms, ".Environment") <- NULL

  mm <- stats::model.matrix(object)
  contrasts <- attr(mm, "contrasts")
  xlevels <- stats::.getXlevels(terms, stats::model.frame(object))
  ranef <- lapply(lme4::ranef(object), as.data.frame)
  re_intercept_only <- all(sapply(ranef, function(x) identical(colnames(x), "(Intercept)")))

  l <- list("coef" = coef,
            "cov" = cov,
            "mse" = stats::sigma(object)^2,
            "xlevels" = xlevels,
            "terms" = terms,
            "contrasts" = contrasts,
            "ranef" = ranef,
            "re_intercept_only" = re_intercept_only)

  class(l) <- "burgle_lmer"

  l
}

## internal
  predict_re_intercept <- function(object, newdata, allow.new.levels = TRUE, model = "burgle_lmer"){
    if(!isTRUE(object$re_intercept_only)){
      stop(paste0("Only intercept-only random effects are currently supported for ", model, " predictions with re.form = NULL"))
    }

    re_lp <- rep(0, nrow(newdata))
    re_names <- names(object$ranef)

    for(g in re_names){
      if(!(g %in% names(newdata))){
        stop(paste0("Grouping variable ", g, " not found in newdata"))
      }
      lev <- as.character(newdata[[g]])
      re_df <- object$ranef[[g]]
      re_vals <- re_df[lev, "(Intercept)"]
      if(any(is.na(re_vals))){
        if(!isTRUE(allow.new.levels)){
          stop("Found new grouping levels in newdata and allow.new.levels = FALSE")
        }
        re_vals[is.na(re_vals)] <- 0
      }
      re_lp <- re_lp + re_vals
    }

    re_lp
  }

  ## internal
  validate_re_form <- function(re.form, model){
    if(is.null(re.form) || (length(re.form) == 1L && is.atomic(re.form) && is.na(re.form))){
      return(invisible(NULL))
    }

    stop(paste0(model, " only supports re.form = NA (fixed effects only) or re.form = NULL (include fitted random effects)"))
  }

  ## internal
  predict_re_lmer <- function(object, newdata, allow.new.levels = TRUE){
    predict_re_intercept(object = object, newdata = newdata, allow.new.levels = allow.new.levels, model = "burgle_lmer")
  }

#' @name predict_burgle
#'
#' @param re.form if NULL include random intercept contributions from fitted groups; if NA (default) use fixed effects only
#' @param allow.new.levels whether unseen grouping levels in newdata are allowed (zero random-effect contribution)
#'
#' @export
predict.burgle_lmer <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "lp", se = FALSE, limits = NULL, seed = NULL, se_type = "prediction", re.form = NA, allow.new.levels = TRUE, ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "response"))

  if(original & draws > 1){
    stop("Can only have one draw from the original model")
  }

  models <- draw_models(object, original = original, draws = draws, seed = seed)

  pn <- simulate_models(object,
                        models = models,
                        newdata = newdata,
                        sims = sims,
                        type = type,
                        se = se,
                        limits = limits,
                        seed = seed,
                        se_type = se_type,
                        re.form = re.form,
                        allow.new.levels = allow.new.levels,
                        ...)

  pn
}

#' Predict for burgle methods
#'
#' @rdname predict_burgle
draw_models <- function(object, original = TRUE, draws = 1, seed = NULL){
  if(original){
    models <- object$coef
  }else{
    if(draws <1|is.na(draws)){stop("draws must be at least 1")}
    set.seed(seed = seed)
    models <- MASS::mvrnorm(n = draws, mu = object$coef, Sigma = object$cov)
  }
  return(models)
}

#' @rdname simulate_models
#'
#' @param re.form if NULL include random intercept contributions from fitted groups; if NA use fixed effects only
#' @param allow.new.levels whether unseen grouping levels in newdata are allowed (zero random-effect contribution)
#'
#' @export
simulate_models.burgle_lmer <- function(object, models = NULL, newdata, type = "lp", sims =1, seed = NULL, se = FALSE, limits = NULL, se_type = "prediction", re.form = NA, allow.new.levels = TRUE, ...){
  if(is.null(models)) stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  se_type <- match.arg(tolower(se_type), c("prediction", "confidence"))
  validate_re_form(re.form = re.form, model = "burgle_lmer")

  mm <- stats::model.matrix(object$terms, data = newdata, xlev = object$xlevels, contrasts.arg = object$contrasts)

  if(!is.null(dim(models))){
    preds <- fastmm(mm, t(models))
  }else{
    preds <- fastmm(mm, matrix(models))
  }

  if(is.null(re.form)){
   re_lp <- predict_re_lmer(object, newdata = newdata, allow.new.levels = allow.new.levels)
   preds <- sweep(preds, 1L, re_lp, FUN = "+")
  }

  if(type == "lp"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  se_p <- rowSums(fastmm(mm, object$cov) * mm)
  if(se_type == "prediction") se_p <- sqrt(se_p + object$mse)

  if(is.null(limits)){
    pn <- simulate_responses(preds, sims, se, se_p)
  }else{
    pn <- simulate_responses_limits(preds, sims, se, se_p, limits = limits)
  }

  pn <- drop_list(pn)

  pn
}
