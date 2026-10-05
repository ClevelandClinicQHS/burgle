#' Absolute value recipe transformation
#'
#' `step_abs()` is a burgle-owned recipes extension that replaces selected
#' numeric columns with their absolute values using [base::abs()]. It does not
#' add columns, offsets, or fitted transformation parameters.
#'
#' @param recipe A recipe object.
#' @param ... Recipe selectors for numeric columns.
#' @param role The role of the transformed columns; defaults to preserving their
#'   existing roles.
#' @param trained Whether the step has been trained.
#' @param columns The selected column names, populated during `prep()`.
#' @param skip Whether to skip this step when baking new data.
#' @param id A unique step identifier.
#' @return An updated recipe, with an absolute-value transformation step.
#' @details Negative infinity becomes positive infinity, missing values remain
#'   missing, and zero remains zero. Unlike the other supported transformation
#'   steps, this constructor is provided by burgle, not recipes.
#' @md
#' @export
step_abs <- function(recipe, ..., role = NA, trained = FALSE, columns = NULL,
                     skip = FALSE, id = recipes::rand_id("abs")) {
  workflow_recipe_dependency("recipes", "step_abs")
  workflow_recipe_dependency("rlang", "step_abs")
  recipes::add_step(
    recipe,
    step_abs_new(terms = rlang::enquos(...), role = role, trained = trained,
                 columns = columns, skip = skip, id = id)
  )
}

step_abs_new <- function(terms, role, trained, columns, skip, id) {
  recipes::step(subclass = "abs", terms = terms, role = role, trained = trained,
                columns = columns, skip = skip, id = id)
}

#' @exportS3Method recipes::prep
prep.step_abs <- function(x, training, info = NULL, ...) {
  columns <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, columns, drop = FALSE],
                      types = c("double", "integer"))
  step_abs_new(terms = x$terms, role = x$role, trained = TRUE,
               columns = columns, skip = x$skip, id = x$id)
}

#' @exportS3Method recipes::bake
bake.step_abs <- function(object, new_data, ...) {
  columns <- workflow_recipe_columns(object$columns)
  recipes::check_new_data(columns, object, new_data)
  for (column in columns) new_data[[column]] <- abs(new_data[[column]])
  new_data
}

#' @export
print.step_abs <- function(x, width = max(20, getOption("width") - 35), ...) {
  recipes::print_step(x$columns, x$terms, x$trained,
                      "Absolute value transformation on ", width)
  invisible(x)
}

#' @exportS3Method recipes::tidy
tidy.step_abs <- function(x, ...) {
  workflow_recipe_dependency("tibble", "step_abs")
  columns <- if (isTRUE(x$trained)) {
    workflow_recipe_columns(x$columns)
  } else recipes::sel2char(x$terms)
  tibble::tibble(terms = columns, id = rep(x$id, length(columns)))
}
