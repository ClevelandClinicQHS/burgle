#' Burgle a fitted logistic regression workflow with spline preprocessing
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

  engine <- workflows::extract_fit_engine(object)

  if (!inherits(engine, "glm") ||
      !identical(engine$family$family, "binomial")) {
    stop("Only binomial glm logistic regression workflows are supported.")
  }

  recipe <- tryCatch(
    workflows::extract_recipe(object, estimated = TRUE),
    error = function(e) NULL
  )

  if (is.null(recipe)) {
    stop("Only workflows with recipe preprocessors are supported.")
  }

  specs <- workflow_spline_specs(recipe)

  if (length(specs) == 0L) {
    stop(
      "The workflow must contain at least one step_bs() or step_ns() step."
    )
  }

  old_terms <- stats::delete.response(engine$terms)
  attr(old_terms, ".Environment") <- baseenv()

  old_labels <- attr(old_terms, "term.labels")

  if (any(attr(old_terms, "order") > 1L)) {
    stop(
      "Interaction terms are not supported by the spline workflow method."
    )
  }

  spline_columns <- unlist(
    lapply(specs, `[[`, "columns"),
    use.names = FALSE
  )

  missing_columns <- setdiff(spline_columns, old_labels)

  if (length(missing_columns) > 0L) {
    stop(
      "Spline columns absent from fitted model: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  spline_variables <- vapply(
    specs,
    `[[`,
    character(1),
    "variable"
  )

  duplicated_variables <- intersect(spline_variables, old_labels)

  if (length(duplicated_variables) > 0L) {
    stop(
      "Spline input variables are also present as ordinary model terms: ",
      paste(duplicated_variables, collapse = ", "),
      ". Use keep_original_cols = FALSE."
    )
  }

  old_probe <- engine$model

  if (is.null(old_probe)) {
    stop(
      "The fitted glm does not contain its model frame. Refit the workflow with model = TRUE."
    )
  }

  old_probe <- old_probe[1L, , drop = FALSE]
  attr(old_probe, "terms") <- NULL
  old_probe <- as.data.frame(old_probe)

  new_probe <- old_probe

  for (spec in specs) {
    new_probe[[spec$variable]] <- mean(spec$boundary)
  }

  old_matrix <- stats::model.matrix(
    old_terms,
    data = old_probe,
    contrasts.arg = engine$contrasts,
    xlev = engine$xlevels
  )

  old_coef <- stats::coef(engine)

  if (!identical(colnames(old_matrix), names(old_coef))) {
    stop(
      "The fitted model matrix columns do not match the fitted coefficients."
    )
  }

  first_spline_columns <- setNames(
    specs |> lapply(identity),
    vapply(
      specs,
      function(spec) spec$columns[[1L]],
      character(1)
    )
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

  new_formula <- workflow_terms_formula(
    labels = labels,
    intercept = identical(attr(old_terms, "intercept"), 1L)
  )

  new_terms <- stats::terms(new_formula)
  attr(new_terms, ".Environment") <- baseenv()

  new_matrix <- stats::model.matrix(
    new_terms,
    data = new_probe,
    contrasts.arg = engine$contrasts,
    xlev = engine$xlevels
  )

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

    old_term_indices <- vapply(
      group,
      function(label) match(label, old_labels),
      integer(1)
    )

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
    stop(
      "Could not align the original coefficients with the compiled terms."
    )
  }

  fitted_indices <- old_order[match(seq_len(ncol(new_matrix)), new_order)]

  out <- burgle(engine)

  out$coef <- out$coef[fitted_indices]
  names(out$coef) <- colnames(new_matrix)

  out$cov <- out$cov[fitted_indices, fitted_indices, drop = FALSE]
  dimnames(out$cov) <- list(
    colnames(new_matrix),
    colnames(new_matrix)
  )

  out$terms <- new_terms

  class(out) <- c("burgle_workflow", class(out))

  out
}

workflow_require_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Install '", pkg, "' to burgle a workflow.")
  }
}

workflow_constant_call <- function(x) {
  if (length(x) == 0L) {
    return(call("numeric", 0L))
  }

  if (length(x) == 1L) {
    return(x)
  }

  as.call(
    c(
      list(as.name("c")),
      as.list(x)
    )
  )
}

workflow_spline_call <- function(spec) {
  spline_fun <- call(
    "::",
    as.name("splines"),
    as.name(spec$kind)
  )

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
      function(left, right) {
        call("+", left, right)
      },
      labels
    )

    if (!intercept) {
      rhs <- call("-", rhs, 1L)
    }
  }

  stats::as.formula(
    call("~", rhs),
    env = baseenv()
  )
}

workflow_spline_specs <- function(recipe) {
  specs <- list()

  for (step in recipe$steps) {
    kind <- if (inherits(step, "step_bs")) {
      "bs"
    } else if (inherits(step, "step_ns")) {
      "ns"
    } else {
      NA_character_
    }

    if (is.na(kind)) {
      stop(
        "Unsupported recipe step: ",
        class(step)[1L],
        ". Only step_bs and step_ns are supported for now."
      )
    }

    if (!isTRUE(step$trained) ||
        isTRUE(step$skip) ||
        is.null(step$objects)) {
      stop(
        "Spline steps must be trained and cannot use skip = TRUE."
      )
    }

    for (variable in names(step$objects)) {
      basis <- step$objects[[variable]]

      knots <- as.numeric(attr(basis, "knots"))
      boundary <- as.numeric(attr(basis, "Boundary.knots"))
      intercept <- attr(basis, "intercept")

      degree <- if (identical(kind, "bs")) {
        as.integer(attr(basis, "degree"))
      } else {
        NULL
      }

      if (is.null(knots) ||
          length(boundary) != 2L ||
          is.null(intercept) ||
          (identical(kind, "bs") && is.null(degree))) {
        stop(
          "Missing trained spline parameters for: ",
          variable
        )
      }

      specs[[length(specs) + 1L]] <- list(
        variable = variable,
        kind = kind,
        knots = knots,
        boundary = boundary,
        intercept = intercept,
        degree = degree,
        columns = paste(
          variable,
          kind,
          seq_len(ncol(basis)),
          sep = "_"
        )
      )
    }
  }

  specs
}

#' @rdname predict_burgle
#' @export
predict.burgle_workflow <- function(
    object,
    newdata,
    original = TRUE,
    draws = 1,
    sims = 1,
    type = "lp",
    se = FALSE,
    seed = NULL,
    ...) {
  if (!is.data.frame(newdata)) {
    stop("newdata must be a data.frame.")
  }

  predict.burgle_glm(
    object = object,
    newdata = newdata,
    original = original,
    draws = draws,
    sims = sims,
    type = type,
    se = se,
    seed = seed,
    ...
  )
}
