#' Burgle a fitted workflow with spline preprocessing
#'
#' Compiles trained `step_bs()` and `step_ns()` steps into a standalone
#' `terms` object. The workflow and recipe are not retained.
#'
#' Only `step_bs()` and `step_ns()` recipe steps are supported at present.
#'
#' @rdname burgle_
#' @export
burgle.workflow <- function(object, ...) {
  if (!missing(...)) {
    dots <- list(...)
    if (length(dots) > 0L) {
      stop("`...` must be empty for burgle.workflow()")
    }
  }

  workflow_require_namespace("workflows")

  extract_fit_engine <- workflow_namespace_function("workflows", "extract_fit_engine")
  extract_preprocessor <- workflow_namespace_function("workflows", "extract_preprocessor")
  extract_recipe <- workflow_namespace_function("workflows", "extract_recipe")
  extract_mold <- workflow_namespace_function("workflows", "extract_mold")

  preprocessor <- extract_preprocessor(object)
  if (inherits(preprocessor, "formula")) {
    stop("Only workflows with recipe preprocessors are supported.")
  }

  if (!inherits(preprocessor, "recipe")) {
    stop("Only workflows with recipe preprocessors are supported.")
  }

  engine <- extract_fit_engine(object)
  burgled <- workflow_burgle_engine(engine)

  if (!workflow_supports_compiled_terms(burgled)) {
    stop(
      "Workflow burgling is not supported for fitted engine class `",
      class(engine)[1],
      "` because the corresponding burgle method does not predict from a replaceable `terms` object."
    )
  }

  recipe_trained <- extract_recipe(object, estimated = TRUE)
  recipe_untrained <- extract_recipe(object, estimated = FALSE)
  mold <- extract_mold(object)

  spline_info <- workflow_spline_specs(recipe_trained)
  specs <- spline_info$specs
  workflow_warn_skipped_steps(spline_info$skipped_steps)

  if (length(specs) == 0L) {
    stop("The workflow must contain at least one step_bs() or step_ns() step.")
  }

  old_terms <- stats::delete.response(burgled$terms)
  attr(old_terms, ".Environment") <- baseenv()
  old_labels <- attr(old_terms, "term.labels")

  if (any(attr(old_terms, "order") > 1L)) {
    stop("Interaction terms are not supported by the spline workflow method.")
  }

  spline_columns <- unlist(lapply(specs, `[[`, "columns"), use.names = FALSE)
  missing_columns <- setdiff(spline_columns, old_labels)
  if (length(missing_columns) > 0L) {
    stop(
      "Spline columns absent from fitted model: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  spline_variables <- vapply(specs, `[[`, character(1), "variable")
  duplicated_variables <- intersect(spline_variables, old_labels)
  if (length(duplicated_variables) > 0L) {
    stop(
      "Spline input variables are also present as ordinary model terms: ",
      paste(duplicated_variables, collapse = ", "),
      ". Use keep_original_cols = FALSE."
    )
  }

  raw_training <- recipe_untrained$template
  if (is.null(raw_training) || !is.data.frame(raw_training) || nrow(raw_training) == 0L) {
    stop(
      "burgle.workflow() needs the original recipe template rows to validate the compiled terms against the baked training design matrix."
    )
  }

  baked_predictors <- mold$predictors
  if (!is.data.frame(baked_predictors)) {
    stop("The fitted workflow predictors must be available as a data frame.")
  }

  raw_training_used <- workflow_training_rows(raw_training, baked_predictors)

  first_spline_columns <- setNames(
    specs |> lapply(identity),
    vapply(specs, function(spec) spec$columns[[1L]], character(1))
  )

  labels <- list()
  old_groups <- list()

  for (label in old_labels) {
    spec <- first_spline_columns[[label]]

    if (!is.null(spec)) {
      labels[[length(labels) + 1L]] <- workflow_spline_call(spec)
      old_groups[[length(old_groups) + 1L]] <- spec$columns
      next
    }

    if (label %in% spline_columns) {
      next
    }

    labels[[length(labels) + 1L]] <- str2lang(label)
    old_groups[[length(old_groups) + 1L]] <- label
  }

  compiled_formula <- workflow_terms_formula(
    labels = labels,
    intercept = identical(attr(old_terms, "intercept"), 1L)
  )

  compiled_terms <- stats::terms(compiled_formula)
  attr(compiled_terms, ".Environment") <- baseenv()

  mf <- stats::model.frame(compiled_formula, data = raw_training_used, na.action = stats::na.pass)
  xlevels <- workflow_get_xlevels(compiled_terms, mf)

  contrasts <- burgled$contrasts
  if (length(contrasts) > 0L) {
    contrasts <- contrasts[intersect(names(contrasts), names(xlevels))]
  }

  old_matrix <- workflow_model_matrix(
    burgled,
    old_terms,
    data = baked_predictors,
    xlev = burgled$xlevels,
    contrasts.arg = burgled$contrasts
  )

  new_matrix <- workflow_model_matrix(
    burgled,
    compiled_terms,
    data = raw_training_used,
    xlev = xlevels,
    contrasts.arg = contrasts
  )

  column_map <- workflow_validate_design_matrices(
    old_matrix,
    new_matrix,
    old_labels = old_labels,
    new_labels = attr(compiled_terms, "term.labels"),
    old_groups = old_groups
  )

  out <- workflow_align_burgled_object(
    burgled = burgled,
    old_matrix = old_matrix,
    new_matrix = new_matrix,
    column_map = column_map
  )

  out$terms <- compiled_terms
  out$xlevels <- xlevels
  out$contrasts <- contrasts

  class(out) <- c("burgle_workflow", class(out))
  out
}

workflow_require_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Install '", pkg, "' to burgle a workflow.")
  }
}

workflow_namespace_function <- function(pkg, fun) {
  getExportedValue(pkg, fun)
}

workflow_burgle_engine <- function(engine) {
  out <- tryCatch(
    burgle(engine),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    stop(
      "burgle.workflow() could not burgle the extracted workflow engine of class `",
      paste(class(engine), collapse = "/"),
      "`: ",
      conditionMessage(out)
    )
  }

  out
}

workflow_supports_compiled_terms <- function(object) {
  inherits(object, c(
    "burgle_lm",
    "burgle_glm",
    "burgle_multinom",
    "burgle_coxph",
    "burgle_cph",
    "burgle_flexsurvreg"
  ))
}

workflow_constant_call <- function(x) {
  if (length(x) == 0L) {
    return(call("numeric", 0L))
  }

  if (length(x) == 1L) {
    return(x)
  }

  as.call(c(list(as.name("c")), as.list(x)))
}

workflow_spline_call <- function(spec) {
  spline_fun <- call("::", as.name("splines"), as.name(spec$kind))

  arguments <- list(
    spline_fun,
    as.name(spec$variable),
    knots = workflow_constant_call(spec$knots),
    Boundary.knots = workflow_constant_call(spec$boundary),
    intercept = spec$intercept
  )

  if (identical(spec$kind, "bs")) {
    arguments$degree <- spec$degree
  }

  as.call(arguments)
}

workflow_terms_formula <- function(labels, intercept = TRUE) {
  if (length(labels) == 0L) {
    rhs <- if (intercept) 1L else 0L
  } else {
    rhs <- Reduce(
      function(left, right) call("+", left, right),
      labels
    )

    if (!intercept) {
      rhs <- call("-", rhs, 1L)
    }
  }

  stats::as.formula(call("~", rhs), env = baseenv())
}

workflow_warn_skipped_steps <- function(skipped_steps) {
  if (length(skipped_steps) == 0L) {
    return(invisible(NULL))
  }

  warning(
    "Unsupported recipe step(s) were skipped during workflow burgling: ",
    paste(sprintf("`%s`", skipped_steps), collapse = ", "),
    ". Only trained `step_bs()` and `step_ns()` steps are compiled into the burgled `terms` object.",
    call. = FALSE
  )
}

workflow_spline_specs <- function(recipe) {
  specs <- list()
  skipped_steps <- character()

  for (step in recipe$steps) {
    kind <- if (inherits(step, "step_bs")) {
      "bs"
    } else if (inherits(step, "step_ns")) {
      "ns"
    } else {
      NA_character_
    }

    if (is.na(kind)) {
      skipped_steps <- unique(c(skipped_steps, class(step)[1L]))
      next
    }

    if (!isTRUE(step$trained) || isTRUE(step$skip) || is.null(step$objects)) {
      stop("Spline steps must be trained and cannot use skip = TRUE.")
    }

    for (variable in names(step$objects)) {
      basis <- step$objects[[variable]]
      knots <- as.numeric(attr(basis, "knots"))
      boundary <- as.numeric(attr(basis, "Boundary.knots"))
      intercept <- attr(basis, "intercept")
      degree <- if (identical(kind, "bs")) as.integer(attr(basis, "degree")) else NULL

      if (length(boundary) != 2L || is.null(intercept) || (identical(kind, "bs") && is.null(degree))) {
        stop("Missing trained spline parameters for: ", variable)
      }

      specs[[length(specs) + 1L]] <- list(
        variable = variable,
        kind = kind,
        knots = knots,
        boundary = boundary,
        intercept = intercept,
        degree = degree,
        columns = paste(variable, kind, seq_len(ncol(basis)), sep = "_")
      )
    }
  }

  list(
    specs = specs,
    skipped_steps = skipped_steps
  )
}

workflow_training_rows <- function(raw_training, baked_predictors) {
  raw_rows <- rownames(raw_training)
  baked_rows <- rownames(baked_predictors)

  if (!is.null(raw_rows) && !is.null(baked_rows) &&
      length(baked_rows) > 0L &&
      !anyDuplicated(raw_rows) &&
      !anyDuplicated(baked_rows) &&
      all(baked_rows %in% raw_rows)) {
    return(raw_training[match(baked_rows, raw_rows), , drop = FALSE])
  }

  if (nrow(raw_training) == nrow(baked_predictors)) {
    return(raw_training)
  }

  stop(
    "burgle.workflow() could not determine which training rows reached the fitted engine after recipe preprocessing."
  )
}

workflow_get_xlevels <- function(terms, model_frame) {
  vars <- attr(terms, "dataClasses")
  vars <- vars[names(vars) != "(response)"]
  factor_vars <- names(vars)[vars %in% c("factor", "ordered")]

  setNames(
    lapply(factor_vars, function(x) levels(model_frame[[x]])),
    factor_vars
  )
}

workflow_model_matrix <- function(object, terms, data, xlev, contrasts.arg) {
  mm <- stats::model.matrix(
    terms,
    data = data,
    xlev = xlev,
    contrasts.arg = contrasts.arg
  )

  if (inherits(object, c("burgle_coxph", "burgle_cph", "burgle_flexsurvreg"))) {
    keep <- attr(mm, "assign") != 0L
    mm <- mm[, keep, drop = FALSE]
    attr(mm, "assign") <- attr(mm, "assign")[keep]
  }

  mm
}

workflow_validate_design_matrices <- function(old_matrix, new_matrix, old_labels, new_labels, old_groups) {
  if (!identical(nrow(old_matrix), nrow(new_matrix))) {
    stop("The old and compiled model matrices have different row counts.")
  }

  old_assign <- attr(old_matrix, "assign")
  new_assign <- attr(new_matrix, "assign")

  old_order <- integer()
  new_order <- integer()

  old_intercept <- which(old_assign == 0L)
  new_intercept <- which(new_assign == 0L)

  if (length(old_intercept) != length(new_intercept)) {
    stop("The old and compiled model matrices have different intercepts.")
  }

  old_order <- c(old_order, old_intercept)
  new_order <- c(new_order, new_intercept)

  for (term_index in seq_along(old_groups)) {
    group <- old_groups[[term_index]]
    old_term_indices <- vapply(group, function(label) match(label, old_labels), integer(1))
    old_columns <- which(old_assign %in% old_term_indices)
    new_columns <- which(new_assign == term_index)

    if (length(old_columns) != length(new_columns)) {
      stop(
        "The compiled term has a different number of model-matrix columns than the original fitted term."
      )
    }

    old_order <- c(old_order, old_columns)
    new_order <- c(new_order, new_columns)
  }

  if (length(old_order) != ncol(old_matrix) ||
      length(new_order) != ncol(new_matrix) ||
      anyDuplicated(old_order) ||
      anyDuplicated(new_order)) {
    stop("Could not align the original coefficients with the compiled terms.")
  }

  list(old_order = old_order, new_order = new_order)
}

workflow_align_burgled_object <- function(burgled, old_matrix, new_matrix, column_map) {
  if (inherits(burgled, "burgle_multinom")) {
    return(workflow_align_multinom_object(burgled, old_matrix, new_matrix, column_map))
  }

  if (inherits(burgled, "burgle_flexsurvreg")) {
    return(workflow_align_flexsurvreg_object(burgled, old_matrix, new_matrix, column_map))
  }

  workflow_align_simple_object(burgled, old_matrix, new_matrix, column_map)
}

workflow_align_simple_object <- function(object, old_matrix, new_matrix, column_map) {
  coef <- object$coef
  cov <- object$cov

  if (!all(colnames(old_matrix) %in% names(coef))) {
    stop("The fitted model matrix columns do not match the fitted coefficients.")
  }

  coef <- coef[colnames(old_matrix)]
  cov <- cov[colnames(old_matrix), colnames(old_matrix), drop = FALSE]

  coef_new <- unname(coef[column_map$old_order])
  names(coef_new) <- colnames(new_matrix)[column_map$new_order]

  cov_new <- cov[column_map$old_order, column_map$old_order, drop = FALSE]
  rownames(cov_new) <- colnames(new_matrix)[column_map$new_order]
  colnames(cov_new) <- colnames(new_matrix)[column_map$new_order]

  object$coef <- coef_new
  object$cov <- cov_new
  object
}

workflow_align_multinom_object <- function(object, old_matrix, new_matrix, column_map) {
  rnl <- length(object$rlev) - 1L
  p <- ncol(old_matrix)

  if (length(object$coef) != rnl * p) {
    stop("Could not align multinom coefficients with the compiled terms.")
  }

  perm <- integer(length(object$coef))
  new_names <- character(length(object$coef))
  block_names <- colnames(new_matrix)[column_map$new_order]

  for (i in seq_len(rnl)) {
    block <- ((i - 1L) * p + 1L):(i * p)
    perm[block] <- block[column_map$old_order]
    new_names[block] <- paste0(object$rlev[[i + 1L]], ":", block_names)
  }

  object$coef <- unname(object$coef[perm])
  names(object$coef) <- new_names
  object$cov <- object$cov[perm, perm, drop = FALSE]
  rownames(object$cov) <- new_names
  colnames(object$cov) <- new_names
  object
}

workflow_align_flexsurvreg_object <- function(object, old_matrix, new_matrix, column_map) {
  parameter_indices <- flexsurv_pars_indices(object)
  if (is.null(parameter_indices)) {
    stop("Could not identify the flexsurv distribution parameter indices.")
  }

  covariate_indices <- setdiff(seq_along(object$coef), parameter_indices)
  if (length(covariate_indices) != ncol(old_matrix)) {
    stop("Could not align flexsurv coefficients with the compiled terms.")
  }

  perm <- seq_along(object$coef)
  perm[covariate_indices] <- covariate_indices[column_map$old_order]

  object$coef <- object$coef[perm]
  names(object$coef)[covariate_indices] <- colnames(new_matrix)[column_map$new_order]
  object$cov <- object$cov[perm, perm, drop = FALSE]
  rownames(object$cov) <- names(object$coef)
  colnames(object$cov) <- names(object$coef)
  object
}

#' @rdname predict_burgle
#' @export
predict.burgle_workflow <- function(object, newdata, ...) {
  class(object) <- setdiff(class(object), "burgle_workflow")
  stats::predict(object, newdata = newdata, ...)
}
