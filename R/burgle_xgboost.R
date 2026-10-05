#' Burgle an XGBoost model
#'
#' Stores the fitted booster as a losslessly compressed UBJSON model buffer,
#' without its training data, evaluation log, callbacks, or call. Predictions are delegated
#' to XGBoost, retaining its output shapes and prediction options.
#'
#' @param object A fitted \code{xgb.Booster} or \code{xgboost} model.
#' @param ... For \code{burgle}, unused. For \code{predict}, arguments passed to
#'   the original XGBoost prediction method.
#' @param newdata Input accepted by the original XGBoost prediction method.
#'   Preprocessing and column order must match training.
#'
#' @details
#' Requires the optional \pkg{xgboost} package (version 1.7.0 or later).
#' Both \code{xgb.train} boosters and \code{xgboost} models are supported.
#' On XGBoost 3.x, the high-level model's response labels and prediction
#' metadata are retained. Early-stopping information is stored in the model.
#' Only prediction-related R metadata is retained. Model buffers are compressed
#' with \code{memCompress(type = "xz")} when this reduces their in-memory size.
#' Prediction decompresses and reloads the booster; existing uncompressed
#' \code{burgle_xgboost} objects remain supported.
#' No parameter-uncertainty draws or response simulations are provided.
#' Model buffers do not contain the training matrix, but fitted trees,
#' feature names, and response labels can still contain sensitive information.
#'
#' @return \code{burgle} returns a \code{burgle_xgboost} object.
#'   \code{predict} returns the same predictions as the original model.
#' @name burgle_xgboost
#' @export
burgle.xgb.Booster <- function(object, ...){
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required to burgle XGBoost models.")
  }

  metadata <- list()
  if ("as_booster" %in% names(formals(xgboost::xgb.load.raw))) {
    complete <- getExportedValue("xgboost", "xgb.Booster.complete")
    object <- complete(object, saveraw = FALSE)
    metadata <- object[intersect(c("feature_names", "params"), names(object))]
    metadata$params <- metadata$params[intersect(c("booster", "nthread"),
                                                 names(metadata$params))]
  } else if (inherits(object, "xgboost")) {
    attrs <- attributes(object)
    metadata$metadata <- attrs$metadata[intersect(c("y_levels", "y_names"),
                                                 names(attrs$metadata))]
    metadata$params <- attrs$params[intersect(c("quantile_alpha", "expectile_alpha"),
                                             names(attrs$params))]
  }
  metadata <- metadata[lengths(metadata) > 0L]

  raw <- xgboost::xgb.save.raw(object, raw_format = "ubj")
  compressed <- memCompress(raw, type = "xz")
  attr(compressed, "compression") <- "xz"
  if (utils::object.size(compressed) < utils::object.size(raw)) raw <- compressed

  l <- list(raw = raw,
            metadata = metadata,
            model_class = if (inherits(object, "xgboost")) "xgboost" else "xgb.Booster")
  class(l) <- "burgle_xgboost"
  l
}

#' @rdname burgle_xgboost
#' @export
burgle.xgboost <- function(object, ...){
  burgle.xgb.Booster(object, ...)
}

#' @rdname burgle_xgboost
#' @export
predict.burgle_xgboost <- function(object, newdata, ...){
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required to predict from burgled XGBoost models.")
  }

  raw <- object$raw
  compression <- attr(raw, "compression", exact = TRUE)
  if (!is.null(compression)) raw <- memDecompress(raw, type = compression)
  loader <- xgboost::xgb.load.raw
  model <- if ("as_booster" %in% names(formals(loader))) {
    loader(raw, as_booster = TRUE)
  } else {
    loader(raw)
  }
  if ("as_booster" %in% names(formals(loader))) {
    for (nm in names(object$metadata)) model[[nm]] <- object$metadata[[nm]]
  } else {
    for (nm in names(object$metadata)) attr(model, nm) <- object$metadata[[nm]]
  }
  if (object$model_class == "xgboost") {
    class(model) <- c("xgboost", "xgb.Booster")
  }

  stats::predict(model, newdata = newdata, ...)
}
