#' Burgle a fitted workflow
#'
#' Compile a trained recipe and burgle the fitted engine using its existing
#' method. Prediction evaluates the compiled transformations, then dispatches
#' to the engine's burgled prediction method, with all arguments unchanged.
#'
#' @param object A fitted `workflows::workflow()` with a recipe preprocessor.
#' @param ... Arguments passed to the underlying engine's `burgle()` method.
#' @return A `burgle_workflow` containing a burgled model and compact trained
#'   preprocessing metadata, not the original workflow or recipe.
#' @details
#' Supported engines are `lm`, `glm`, `multinom`, `coxph`, `cph`,
#' `flexsurvreg`, `CauseSpecificCox`, and `rfsrc`. Engine availability in
#' tidymodels is separate from burgle support; see the workflow documentation.
#'
#' Compiled recipes steps are `step_poly()`, `step_ns()`, `step_bs()`,
#' `step_interact()`, `step_spline_b()`, `step_spline_natural()`,
#' `step_harmonic()`, `step_poly_bernstein()`, `step_spline_monotone()`,
#' `step_spline_convex()`, `step_spline_nonnegative()`, `step_log()`,
#' `step_sqrt()`, `step_inverse()`, `step_invlogit()`, `step_logit()`,
#' `step_ratio()`, and `step_lag()`. The package also supplies [step_abs()]
#' as a recipes extension. Basis parameters are learned at preparation time,
#' not recomputed from prediction data. The compiler preserves execution order,
#' column selections, names and original-column retention.
#'
#' Workflow extraction needs workflows and recipes. Prediction does not:
#' splines2 bases require splines2, and lags require dplyr; other operations use
#' base R, stats or splines. Underlying model runtime dependencies still apply.
#' Serialize the result with `saveRDS()` and restore it with `readRDS()`.
#'
#' Model-specific prediction arguments and outputs are not translated into
#' parsnip conventions. For example, burgle GLM `type = "link"` returns
#' inverse-link values, whereas binomial `type = "response"` samples outcomes.
#' Aliased coefficients, missing rows and novel levels follow the underlying
#' burgled model contract. Fitted terms and all parameter structures are kept
#' in their original order rather than inferred from a new design matrix.
#' Default nnet fits have their Hessian reconstructed transiently from trained
#' predictors. Fits collapsing rows with `summ` need `Hess = TRUE` at fitting.
#' Recipes does not retain global interaction contrast options: keep those
#' options unchanged between fitting and burgling. Compiled contrasts are
#' numeric matrices and do not depend on subsequent global option changes.
#'
#' Formula and variable preprocessors, matrix recipe blueprints, and workflow
#' postprocessors are not supported. Use a recipe (possibly with zero steps)
#' and the default data-frame blueprint. Unsupported recipe steps warn and
#' are omitted; their required outputs must be supplied externally, and even
#' structurally valid inputs may differ in meaning from the training inputs.
#'
#' Lags operate on each prediction batch independently, in its supplied row
#' order, without grouping, sorting, history, or cross-call state.
#'
#' @md
#' @export
burgle.workflow <- function(object, ...) {
  if (!requireNamespace("workflows", quietly = TRUE)) {
    stop("Burgling workflows requires the 'workflows' package.", call. = FALSE)
  }
  if (!isTRUE(object$trained)) {
    stop("The workflow must be fitted before burgling.", call. = FALSE)
  }
  if (length(object$post$actions)) {
    stop("Workflow postprocessors are not supported; burgle the fitted engine instead.",
         call. = FALSE)
  }
  recipe <- tryCatch(
    workflows::extract_recipe(object, estimated = TRUE),
    error = function(e) {
      stop("Workflow preprocessing requires a recipe, including for zero-step workflows. ",
           "Formula and variable preprocessors are not compiled; use add_recipe().",
           call. = FALSE)
    }
  )
  mold <- workflows::extract_mold(object)
  if (!is.data.frame(mold$predictors)) {
    stop("Workflow recipe blueprints must produce a data frame or tibble, not a matrix.",
         call. = FALSE)
  }
  engine <- workflows::extract_fit_engine(object)
  supported <- vapply(class(engine), function(cl) {
    !is.null(utils::getS3method("burgle", cl, optional = TRUE))
  }, logical(1))
  if (!any(supported)) {
    stop("No supported burgle() method for workflow engine class: ",
         paste(class(engine), collapse = ", "), ".", call. = FALSE)
  }
  preprocessing <- compile_workflow_recipe(recipe)
  engine <- workflow_prepare_engine(engine, mold$predictors)
  model <- burgle(engine, ...)
  new_burgle_workflow(model, preprocessing, names(mold$predictors),
                      intercept = isTRUE(mold$blueprint$intercept))
}

workflow_prepare_engine <- function(engine, predictors) {
  ## nnet's default workflow fit does not retain a Hessian. Its vcov() method
  ## would reconstruct a model frame from a call whose workflow data is gone.
  if (inherits(engine, "multinom") && is.null(engine$Hessian)) {
    data <- as.data.frame(predictors)
    if (length(engine$na.action)) {
      data <- data[-as.integer(engine$na.action), , drop = FALSE]
    }
    if (nrow(data) != nrow(engine$fitted.values)) {
      stop("Cannot reconstruct the multinomial workflow Hessian for collapsed ",
           "training rows. Refit with set_engine('nnet', Hess = TRUE).",
           call. = FALSE)
    }
    matrix <- stats::model.matrix(stats::delete.response(engine$terms),
                                  data = data, xlev = engine$xlevels,
                                  contrasts.arg = engine$contrasts)
    hessian <- get("multinomHess", envir = asNamespace("nnet"), inherits = FALSE)
    engine$Hessian <- hessian(engine, Z = matrix)
  }
  engine
}

new_burgle_workflow <- function(model, preprocessing, predictors,
                                intercept = FALSE) {
  ## Keep the engine's fitted column ordering and all parameter structures.
  ## Removing response terms also handles engines that retain them.
  model <- workflow_clean_terms(model)
  structure(list(model = model, preprocessing = preprocessing,
                 predictors = predictors, intercept = intercept),
            class = "burgle_workflow")
}

workflow_clean_terms <- function(x) {
  if (inherits(x, "terms")) {
    x <- stats::delete.response(x)
    attr(x, ".Environment") <- NULL
    return(x)
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[i] <- list(workflow_clean_terms(x[[i]]))
    }
  }
  x
}

workflow_predictors <- function(object, newdata) {
  if (!is.data.frame(newdata)) {
    stop("newdata must be an object of class data.frame", call. = FALSE)
  }
  data <- eval_workflow_recipe(object$preprocessing, newdata)
  if (isTRUE(object$intercept)) {
    data[["(Intercept)"]] <- rep(1, nrow(data))
  }
  missing <- setdiff(object$predictors, names(data))
  if (length(missing)) {
    stop("Compiled preprocessing is missing fitted model inputs: ",
         paste(missing, collapse = ", "),
         ". Supply these columns externally if their recipe step was omitted ",
         "(unsupported or skip = TRUE), or include the required raw predictors.",
         call. = FALSE)
  }
  data[, object$predictors, drop = FALSE]
}

#' @rdname burgle.workflow
#' @param newdata A data frame of raw predictors, plus externally supplied
#'   inputs required by omitted preprocessing.
#' @export
predict.burgle_workflow <- function(object, newdata, ...) {
  stats::predict(object$model,
                 newdata = workflow_predictors(object, newdata), ...)
}

#' @rdname burgle.workflow
#' @export
predict_time.burgle_workflow <- function(object, newdata, ...) {
  predict_time(object$model,
               newdata = workflow_predictors(object, newdata), ...)
}
