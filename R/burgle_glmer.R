#' @rdname burgle_
#'
#' @export
burgle.glmerMod <- function(object, ...){
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
            "mse" = 0,
            "xlevels" = xlevels,
            "terms" = terms,
            "family" = stats::family(object)$family,
            "inv_link" = stats::family(object)$linkinv,
            "contrasts" = contrasts,
            "ranef" = ranef,
            "re_intercept_only" = re_intercept_only)

  class(l) <- "burgle_glmer"

  l
}

#' @name predict_burgle
#'
#' @export
predict.burgle_glmer <- function(object, newdata, original = TRUE, draws = 1, sims = 1, type = "lp", se = FALSE, seed = NULL, se_type = "prediction", re.form = NA, allow.new.levels = TRUE, ...){
  if(!is.data.frame(newdata)) stop("newdata must be an object of class data.frame")
  type <- match.arg(tolower(type), c("lp", "link", "response"))
  se_type <- match.arg(tolower(se_type), c("prediction", "confidence"))

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
                        seed = seed,
                        se_type = se_type,
                        re.form = re.form,
                        allow.new.levels = allow.new.levels,
                        ...)

  pn
}

#' @rdname simulate_models
#'
#' @export
simulate_models.burgle_glmer <- function(object, models = NULL, newdata, type = "lp", sims = 1, seed = NULL, se = FALSE, se_type = "prediction", re.form = NA, allow.new.levels = TRUE, ...){
  if(is.null(models)) stop("Please specify models using `draw_models()`, otherwise use corresponding predict()")
  se_type <- match.arg(tolower(se_type), c("prediction", "confidence"))

  mm <- stats::model.matrix(object$terms, data = newdata, xlev = object$xlevels, contrasts.arg = object$contrasts)

  if(!is.null(dim(models))){
    preds <- fastmm(mm, t(models))
  }else{
    preds <- fastmm(mm, matrix(models))
  }

  if(is.null(re.form)){
    re_lp <- predict_re_intercept(object = object, newdata = newdata, allow.new.levels = allow.new.levels, model = "burgle_glmer")
    preds <- sweep(preds, 1L, re_lp, FUN = "+")
  }

  if(type == "lp"){
    if(sims > 1L) warning("Only 1 sim is possible for type = 'lp'")
    return(preds)
  }

  se_p <- rowSums(fastmm(mm, object$cov) * mm)
  if(se_type == "prediction") se_p <- sqrt(se_p + object$mse)

  preds <- simulate_responses(preds, sims, se, se_p)
  preds <- lapply(preds, object$inv_link)

  if(type == "link"){
    return(drop_list(preds))
  }

  if(!grepl("binomial", object$family)) stop("please use type = 'link' for model families other than binomial and quasibinomial")

  if(is.list(preds)){
    pn <- lapply(preds, simulate_responses_binom, sims = 1)
  }else{
    pn <- simulate_responses_binom(preds, sims = 1)
  }

  pn <- lapply(pn, drop_list)
  pn <- drop_list(pn)

  pn
}
