#' Burgle a fitted workflow
#'
#' @rdname burgle_
#' @details
#' For fitted `workflows` objects, `burgle()` extracts and burgles the engine
#' fit. `recipes::step_ns()` steps are reconstructed. Each complete
#' additive spline expansion is replaced in the returned `terms` by a
#' `splines::ns()` call with the trained knots, boundary knots, and intercept.
#' Coefficients and covariance entries are reordered to match the reconstructed
#' design matrix. For recipes containing only `step_ns()`, predictions take the
#' predictors supplied to the recipe, not the baked spline columns. The recipe
#' and training data are not retained.
#'
#' Other recipe steps produce a warning and are not reconstructed. Returned
#' terms may require preprocessed predictors and may not reproduce the workflow
#' predictions, especially if other steps modify the spline columns themselves.
#' Skipped spline steps and partial or interacting spline expansions produce an
#' error. Spline reconstruction requires an engine with a
#' burgle method returning model terms, a coefficient vector, and a covariance
#' matrix.
#' @export
burgle.workflow <- function(object, ...) {
  if (!requireNamespace("workflows", quietly = TRUE)) {
    stop("Package 'workflows' is required to burgle a workflow.")
  }

  engine <- workflows::extract_fit_engine(object)
  preprocessor <- workflows::extract_preprocessor(object)
  if (!inherits(preprocessor, "recipe")) {
    return(burgle(engine, ...))
  }

  recipe <- workflows::extract_recipe(object, estimated = TRUE)
  supported <- vapply(recipe$steps, inherits, logical(1), "step_ns")
  if (!all(supported)) {
    unsupported <- unique(vapply(recipe$steps[!supported], function(step) {
      class(step)[1]
    }, character(1)))
    warning("Unsupported recipe steps are not reconstructed: ",
            paste(unsupported, collapse = ", "),
            ". Returned terms may require preprocessed predictors and may not reproduce workflow predictions.",
            call. = FALSE)
  }
  spline_steps <- recipe$steps[supported]
  if (any(vapply(spline_steps, function(step) isTRUE(step$skip), logical(1)))) {
    stop("Skipped step_ns() steps are not supported by burgle.workflow().")
  }

  result <- burgle(engine, ...)
  if (!length(spline_steps)) {
    return(result)
  }
  terms <- result$terms
  if (!inherits(terms, "terms")) terms <- stats::terms(engine)
  if (!inherits(terms, "terms") || !is.numeric(result$coef) ||
      !is.null(dim(result$coef)) || !is.matrix(result$cov)) {
    stop("Spline reconstruction requires model terms, a coefficient vector, and a covariance matrix.")
  }
  legacy_formula <- is.character(result$formula)
  contrasts <- result$contrasts
  if (is.null(contrasts)) contrasts <- engine$contrasts
  terms <- stats::delete.response(terms)
  if (length(attr(terms, "offset"))) {
    stop("Offsets with step_ns() are not supported by burgle.workflow().")
  }
  labels <- attr(terms, "term.labels")
  expressions <- lapply(labels, str2lang)
  groups <- list()
  probe <- as.data.frame(workflows::extract_mold(object)$predictors[1, , drop = FALSE])

  for (step in spline_steps) {
    for (variable in names(step$objects)) {
      basis <- step$objects[[variable]]
      count <- ncol(basis)
      columns <- paste0(variable, "_ns_",
                        formatC(seq_len(count), width = nchar(count), flag = "0"))
      positions <- vapply(columns, function(column) {
        matches <- which(vapply(expressions, identical, logical(1), as.name(column)))
        if (length(matches) == 1L) matches else NA_integer_
      }, integer(1))
      uses <- which(vapply(expressions, function(expr) {
        any(all.vars(expr) %in% columns)
      }, logical(1)))
      if (anyNA(positions) || !setequal(uses, positions) ||
          !all(columns %in% names(probe)) ||
          any(vapply(groups, function(group) variable %in% all.vars(group$call),
                     logical(1)))) {
        stop("step_ns() requires complete, additive spline expansions without name collisions or chained splines.")
      }

      call <- as.call(c(list(quote(splines::ns), as.name(variable)),
                        list(knots = unname(attr(basis, "knots")),
                             Boundary.knots = unname(attr(basis, "Boundary.knots")),
                             intercept = attr(basis, "intercept"))))
      groups[[length(groups) + 1L]] <- list(
        positions = positions, columns = labels[positions], call = call
      )
      probe[[variable]] <- mean(attr(basis, "Boundary.knots"))
    }
  }
  if (!length(groups)) {
    return(result)
  }

  new_expressions <- list()
  column_groups <- list()
  for (i in seq_along(expressions)) {
    group_index <- which(vapply(groups, function(group) i %in% group$positions,
                                logical(1)))
    if (length(group_index)) {
      group <- groups[[group_index]]
      if (i != min(group$positions)) next
      new_expressions[[length(new_expressions) + 1L]] <- group$call
      column_groups[[length(column_groups) + 1L]] <- group$columns
    } else {
      new_expressions[[length(new_expressions) + 1L]] <- expressions[[i]]
      column_groups[length(column_groups) + 1L] <- list(NULL)
    }
  }
  rhs <- if (length(new_expressions)) Reduce(function(x, y) call("+", x, y),
                                           new_expressions) else 1
  if (attr(terms, "intercept") == 0L) rhs <- call("-", rhs, 1)
  formula <- stats::as.formula(call("~", rhs), env = baseenv())
  new_terms <- stats::terms(formula, keep.order = TRUE)
  matrix <- stats::model.matrix(new_terms, data = probe, xlev = result$xlevels,
                                contrasts.arg = contrasts)
  assignment <- attr(matrix, "assign")
  old_names <- colnames(matrix)
  for (i in seq_along(column_groups)) {
    if (!is.null(column_groups[[i]])) {
      old_names[assignment == i] <- column_groups[[i]]
    }
  }
  keep <- old_names %in% names(result$coef)
  if (anyDuplicated(old_names[keep]) ||
      !setequal(old_names[keep], names(result$coef))) {
    stop("Reconstructed spline terms do not match the engine coefficients.")
  }
  new_names <- colnames(matrix)[keep]
  result$coef <- stats::setNames(result$coef[old_names[keep]], new_names)
  result$cov <- result$cov[old_names[keep], old_names[keep], drop = FALSE]
  dimnames(result$cov) <- list(new_names, new_names)
  attr(new_terms, ".Environment") <- NULL
  result$terms <- new_terms
  if (legacy_formula) {
    result$formula <- attr(new_terms, "term.labels")
    if (attr(new_terms, "intercept") == 0L) {
      result$formula <- c(result$formula, "-1")
    }
  }
  result
}
