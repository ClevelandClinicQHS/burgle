#' Burgle workflows
#'
#' @rdname burgle_
#'
#' @export
burgle.workflow <- function(object, ...) {
  if (!missing(...)) {
    dots <- list(...)
    if (length(dots) > 0L) {
      stop("`...` must be empty for burgle.workflow()")
    }
  }

  workflow_require_namespace("workflows")
  workflow_require_namespace("recipes")
  workflow_require_namespace("parsnip")

  extract_fit_parsnip <- workflow_namespace_function("workflows", "extract_fit_parsnip")
  extract_fit_engine <- workflow_namespace_function("workflows", "extract_fit_engine")
  extract_mold <- workflow_namespace_function("workflows", "extract_mold")
  extract_preprocessor <- workflow_namespace_function("workflows", "extract_preprocessor")
  extract_recipe <- workflow_namespace_function("workflows", "extract_recipe")
  extract_spec_parsnip <- workflow_namespace_function("workflows", "extract_spec_parsnip")

  engine <- extract_fit_engine(object)
  spec <- extract_spec_parsnip(object)

  workflow_validate_model_spec(spec, engine)

  preprocessor <- extract_preprocessor(object)

  if (inherits(preprocessor, "formula")) {
    stop(
      "burgle.workflow() does not support formula-preprocessor workflows because hardhat expands them before glm fitting and the raw training data needed to reconstruct factor-aware terms is not retained."
    )
  }

  if (!inherits(preprocessor, "recipe")) {
    stop(
      "burgle.workflow() only supports workflows with recipe or formula preprocessors."
    )
  }

  recipe_trained <- extract_recipe(object, estimated = TRUE)
  recipe_untrained <- extract_recipe(object, estimated = FALSE)
  mold <- extract_mold(object)

  compiled <- workflow_compile_recipe(
    recipe_trained = recipe_trained,
    recipe_untrained = recipe_untrained,
    baked_predictors = mold$predictors,
    engine = engine
  )

  out <- burgle.glm(engine)
  out$coef <- compiled$coef
  out$cov <- compiled$cov
  out$terms <- compiled$terms
  out$xlevels <- compiled$xlevels
  out$contrasts <- compiled$contrasts
  out$workflow_required_pkgs <- compiled$required_pkgs

  class(out) <- c("burgle_workflow", class(out))
  out
}

#' @rdname predict_burgle
#'
#' @export
predict.burgle_workflow <- function(object, newdata, ...) {
  workflow_check_runtime_dependencies(object)
  predict.burgle_glm(object = object, newdata = newdata, ...)
}

workflow_require_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package `", pkg, "` must be installed to use burgle.workflow().")
  }
}

workflow_namespace_function <- function(pkg, fun) {
  getExportedValue(pkg, fun)
}

workflow_validate_model_spec <- function(spec, engine) {
  if (!inherits(spec, "logistic_reg")) {
    stop("burgle.workflow() currently supports only parsnip logistic_reg() workflows.")
  }

  if (!identical(spec$engine, "glm")) {
    stop("burgle.workflow() currently supports only logistic_reg() workflows with the `glm` engine.")
  }

  if (!identical(spec$mode, "classification")) {
    stop("burgle.workflow() currently supports only classification workflows.")
  }

  if (!inherits(engine, "glm")) {
    stop("burgle.workflow() expected a fitted `glm` engine object.")
  }

  if (!identical(engine$family$family, "binomial")) {
    stop("burgle.workflow() currently supports only fitted binomial glm workflows.")
  }

  offsets <- attr(stats::terms(engine), "offset")
  if (!is.null(offsets) && length(offsets) > 0L) {
    stop("Workflow models with formula offsets are not supported.")
  }
}

workflow_compile_recipe <- function(recipe_trained, recipe_untrained, baked_predictors, engine) {
  raw_training <- recipe_untrained$template
  if (is.null(raw_training) || !is.data.frame(raw_training) || nrow(raw_training) == 0L) {
    stop(
      "burgle.workflow() needs the original recipe template rows to validate the compiled terms against the baked training design matrix."
    )
  }

  original_info <- summary(recipe_untrained, original = TRUE)
  predictor_names <- unique(original_info$variable[original_info$role == "predictor"])
  state <- workflow_initial_state(raw_training, predictor_names)

  required_pkgs <- character()

  for (step in recipe_trained$steps) {
    step_class <- class(step)[1]
    step_subclass <- sub("^step_", "", step_class)

    if (isTRUE(step$skip)) {
      next
    }

    step_result <- workflow_compile_step(
      state = state,
      step = step,
      step_class = step_class,
      step_subclass = step_subclass
    )

    state <- step_result$state
    required_pkgs <- unique(c(required_pkgs, step_result$required_pkgs))
  }

  baked_predictor_names <- colnames(baked_predictors)
  missing_predictors <- setdiff(baked_predictor_names, names(state))
  if (length(missing_predictors) > 0L) {
    stop(
      "Compiled workflow validation failed: compiled recipe state is missing baked predictor column(s) ",
      paste(sprintf("`%s`", missing_predictors), collapse = ", "),
      "."
    )
  }
  state <- state[baked_predictor_names]

  compiled_formula <- workflow_terms_formula(
    state = state,
    intercept = isTRUE(attr(stats::delete.response(engine$terms), "intercept") == 1L)
  )

  compiled_terms <- stats::terms(compiled_formula, data = raw_training)
  attr(compiled_terms, ".Environment") <- baseenv()

  mf <- stats::model.frame(compiled_formula, data = raw_training, na.action = stats::na.pass)
  xlevels <- stats::.getXlevels(compiled_terms, mf)

  contrasts <- engine$contrasts
  if (length(contrasts) > 0L) {
    contrasts <- contrasts[intersect(names(contrasts), names(xlevels))]
  }

  old_terms <- stats::delete.response(engine$terms)
  old_mm <- stats::model.matrix(
    old_terms,
    data = baked_predictors,
    xlev = engine$xlevels,
    contrasts.arg = engine$contrasts
  )
  new_mm <- stats::model.matrix(
    compiled_terms,
    data = raw_training,
    xlev = xlevels,
    contrasts.arg = contrasts
  )

  column_map <- workflow_validate_design_matrices(old_mm, new_mm)

  coef <- stats::coef(engine)
  if (anyNA(coef) || !all(colnames(old_mm) %in% names(coef))) {
    stop(
      "burgle.workflow() does not support rank-deficient or aliased workflow fits because the fitted glm coefficients are not fully aligned with the baked design matrix columns."
    )
  }
  coef <- coef[colnames(old_mm)]

  cov <- stats::vcov(engine)
  if (!all(colnames(old_mm) %in% rownames(cov)) || !all(colnames(old_mm) %in% colnames(cov))) {
    stop(
      "burgle.workflow() does not support rank-deficient or aliased workflow fits because the fitted glm covariance matrix is not fully aligned with the baked design matrix columns."
    )
  }
  cov <- cov[colnames(old_mm), colnames(old_mm), drop = FALSE]

  coef_new <- unname(coef[column_map$old_order])
  names(coef_new) <- colnames(new_mm)[column_map$new_order]

  cov_new <- cov[column_map$old_order, column_map$old_order, drop = FALSE]
  rownames(cov_new) <- colnames(new_mm)[column_map$new_order]
  colnames(cov_new) <- colnames(new_mm)[column_map$new_order]

  list(
    coef = coef_new,
    cov = cov_new,
    terms = compiled_terms,
    xlevels = xlevels,
    contrasts = contrasts,
    required_pkgs = unique(required_pkgs)
  )
}

workflow_initial_state <- function(data, predictor_names) {
  predictor_names <- predictor_names[predictor_names %in% names(data)]

  state <- setNames(vector("list", length(predictor_names)), predictor_names)
  for (i in seq_along(predictor_names)) {
    name <- predictor_names[[i]]
    x <- data[[name]]
    scalar <- !is.factor(x) && !is.character(x)

    state[[i]] <- list(
      expr = as.name(name),
      scalar = scalar
    )
  }

  state
}

workflow_compile_step <- function(state, step, step_class, step_subclass) {
  switch(
    step_subclass,
    poly = workflow_compile_step_poly(state, step),
    ns = workflow_compile_step_ns(state, step),
    bs = workflow_compile_step_bs(state, step),
    interact = workflow_compile_step_interact(state, step),
    spline_b = workflow_compile_step_splines2(state, step),
    spline_natural = workflow_compile_step_splines2(state, step),
    harmonic = workflow_compile_step_harmonic(state, step),
    poly_bernstein = workflow_compile_step_splines2(state, step),
    spline_monotone = workflow_compile_step_splines2(state, step),
    spline_convex = workflow_compile_step_splines2(state, step),
    spline_nonnegative = workflow_compile_step_splines2(state, step),
    log = workflow_compile_step_log(state, step),
    sqrt = workflow_compile_step_simple_map(state, step, function(x, step) call("sqrt", x)),
    inverse = workflow_compile_step_simple_map(state, step, workflow_inverse_call),
    invlogit = workflow_compile_step_simple_map(state, step, workflow_invlogit_call),
    logit = workflow_compile_step_simple_map(state, step, workflow_logit_call),
    abs = workflow_compile_step_simple_map(state, step, function(x, step) call("abs", x)),
    ratio = workflow_compile_step_ratio(state, step),
    lag = workflow_stop_step(
      step_class,
      "step_lag() depends on row order and cross-row state, so it cannot be compiled safely for arbitrary prediction batches."
    ),
    workflow_stop_step(
      step_class,
      paste0(
        "Recipe step `", step_class,
        "` is not supported because burgle.workflow() only compiles transformations that can be reproduced losslessly from raw `newdata`."
      )
    )
  )
}

workflow_compile_step_simple_map <- function(state, step, fun) {
  cols <- workflow_step_columns(step)

  for (col in cols) {
    entry <- workflow_get_entry(state, col, class(step)[1])
    if (!isTRUE(entry$scalar)) {
      workflow_stop_step(
        class(step)[1],
        paste0("`", class(step)[1], "` can only be compiled for scalar columns.")
      )
    }
    state[[col]]$expr <- fun(entry$expr, step)
    state[[col]]$scalar <- TRUE
  }

  list(state = state, required_pkgs = character())
}

workflow_compile_step_log <- function(state, step) {
  cols <- workflow_step_columns(step)

  for (col in cols) {
    entry <- workflow_get_entry(state, col, class(step)[1])
    if (!isTRUE(entry$scalar)) {
      workflow_stop_step(
        class(step)[1],
        "step_log() can only be compiled for scalar columns."
      )
    }

    if (isTRUE(step$signed)) {
      expr <- call(
        "ifelse",
        call("<", call("abs", entry$expr), 1),
        0,
        call("*", call("sign", entry$expr), call("log", call("abs", entry$expr), base = step$base))
      )
    } else {
      expr <- call("log", call("+", entry$expr, step$offset), base = step$base)
    }

    state[[col]]$expr <- expr
    state[[col]]$scalar <- TRUE
  }

  list(state = state, required_pkgs = character())
}

workflow_compile_step_poly <- function(state, step) {
  objs <- step$objects
  new_entries <- list()

  for (col in names(objs)) {
    entry <- workflow_get_entry(state, col, class(step)[1])
    poly_call <- workflow_poly_call(entry$expr, objs[[col]])

    for (i in seq_len(ncol(objs[[col]]))) {
      new_name <- paste(col, "poly", i, sep = "_")
      new_entries[[new_name]] <- list(
        expr = workflow_matrix_column_call(poly_call, i),
        scalar = TRUE
      )
    }
  }

  state <- workflow_append_entries(state, new_entries, keep_original = isTRUE(step$keep_original_cols), originals = names(objs), step_class = class(step)[1])

  list(state = state, required_pkgs = character())
}

workflow_compile_step_bs <- function(state, step) {
  objs <- step$objects
  new_entries <- list()

  for (col in names(objs)) {
    entry <- workflow_get_entry(state, col, class(step)[1])
    spline_call <- workflow_bs_call(entry$expr, objs[[col]])

    for (i in seq_len(ncol(objs[[col]]))) {
      new_name <- paste(col, "bs", i, sep = "_")
      new_entries[[new_name]] <- list(
        expr = workflow_matrix_column_call(spline_call, i),
        scalar = TRUE
      )
    }
  }

  state <- workflow_append_entries(state, new_entries, keep_original = isTRUE(step$keep_original_cols), originals = names(objs), step_class = class(step)[1])

  list(state = state, required_pkgs = character())
}

workflow_compile_step_ns <- function(state, step) {
  objs <- step$objects
  new_entries <- list()

  for (col in names(objs)) {
    entry <- workflow_get_entry(state, col, class(step)[1])
    spline_call <- workflow_ns_call(entry$expr, objs[[col]])

    for (i in seq_len(ncol(objs[[col]]))) {
      new_name <- paste(col, "ns", i, sep = "_")
      new_entries[[new_name]] <- list(
        expr = workflow_matrix_column_call(spline_call, i),
        scalar = TRUE
      )
    }
  }

  state <- workflow_append_entries(state, new_entries, keep_original = isTRUE(step$keep_original_cols), originals = names(objs), step_class = class(step)[1])

  list(state = state, required_pkgs = character())
}

workflow_compile_step_splines2 <- function(state, step) {
  results <- step$results
  new_entries <- list()

  for (col in names(results)) {
    entry <- workflow_get_entry(state, col, class(step)[1])
    spline_call <- workflow_splines2_call(entry$expr, results[[col]])
    n_cols <- results[[col]]$dim[[2]]

    for (i in seq_len(n_cols)) {
      new_name <- paste0(results[[col]]$nm, "_", i)
      new_entries[[new_name]] <- list(
        expr = workflow_matrix_column_call(spline_call, i),
        scalar = TRUE
      )
    }
  }

  state <- workflow_append_entries(state, new_entries, keep_original = isTRUE(step$keep_original_cols), originals = names(results), step_class = class(step)[1])

  list(state = state, required_pkgs = "splines2")
}

workflow_compile_step_harmonic <- function(state, step) {
  cols <- names(step$starting_val)
  new_entries <- list()
  n_frequency <- length(step$frequency)

  for (col in cols) {
    entry <- workflow_get_entry(state, col, class(step)[1])
    for (i in seq_len(n_frequency)) {
      freq <- unname(step$frequency[[i]])
      sin_name <- paste0(col, "_sin_", i)
      cos_name <- paste0(col, "_cos_", i)

      new_entries[[sin_name]] <- list(
        expr = workflow_harmonic_call(entry$expr, freq, step$starting_val[[col]], step$cycle_size[[col]], "sin"),
        scalar = TRUE
      )
      new_entries[[cos_name]] <- list(
        expr = workflow_harmonic_call(entry$expr, freq, step$starting_val[[col]], step$cycle_size[[col]], "cos"),
        scalar = TRUE
      )
    }
  }

  state <- workflow_append_entries(state, new_entries, keep_original = isTRUE(step$keep_original_cols), originals = cols, step_class = class(step)[1])

  list(state = state, required_pkgs = character())
}

workflow_compile_step_ratio <- function(state, step) {
  columns <- step$columns
  new_entries <- list()

  for (i in seq_len(nrow(columns))) {
    top <- columns$top[[i]]
    bottom <- columns$bottom[[i]]
    top_entry <- workflow_get_entry(state, top, class(step)[1])
    bottom_entry <- workflow_get_entry(state, bottom, class(step)[1])

    if (!isTRUE(top_entry$scalar) || !isTRUE(bottom_entry$scalar)) {
      workflow_stop_step(
        class(step)[1],
        "step_ratio() can only be compiled when both numerator and denominator resolve to scalar columns at recipe compile time."
      )
    }

    new_name <- step$naming(top, bottom)
    new_entries[[new_name]] <- list(
      expr = call("/", top_entry$expr, bottom_entry$expr),
      scalar = TRUE
    )
  }

  originals <- unique(c(columns$top, columns$bottom))
  state <- workflow_append_entries(state, new_entries, keep_original = isTRUE(step$keep_original_cols), originals = originals, step_class = class(step)[1])

  list(state = state, required_pkgs = character())
}

workflow_compile_step_interact <- function(state, step) {
  objects <- step$objects
  if (is.null(objects)) {
    workflow_stop_step(
      class(step)[1],
      "step_interact() must be trained before compilation; no interaction objects were available."
    )
  }

  new_entries <- list()
  originals <- character()

  for (obj in objects) {
    label <- utils::tail(attr(obj, "term.labels"), 1)
    vars <- strsplit(label, ":", fixed = TRUE)[[1]]
    entries <- lapply(vars, function(x) workflow_get_entry(state, x, class(step)[1]))

    if (!all(vapply(entries, function(x) isTRUE(x$scalar), logical(1)))) {
      workflow_stop_step(
        class(step)[1],
        "step_interact() is only supported when every referenced column is scalar at recipe compile time; interactions involving ordinary factor terms cannot be compiled safely."
      )
    }

    exprs <- lapply(entries, `[[`, "expr")
    interaction_expr <- Reduce(function(x, y) call("*", x, y), exprs)
    new_name <- gsub(":", step$sep, label, fixed = TRUE)

    new_entries[[new_name]] <- list(
      expr = interaction_expr,
      scalar = TRUE
    )

    originals <- unique(c(originals, vars))
  }

  state <- workflow_append_entries(state, new_entries, keep_original = isTRUE(step$keep_original_cols), originals = originals, step_class = class(step)[1])

  list(state = state, required_pkgs = character())
}

workflow_step_columns <- function(step) {
  cols <- step$columns
  if (is.null(cols)) {
    return(character())
  }

  nms <- names(cols)
  if (is.null(nms) || any(nms == "")) {
    return(as.character(unname(cols)))
  }

  nms
}

workflow_append_entries <- function(state, entries, keep_original, originals, step_class) {
  if (length(entries) == 0L) {
    if (keep_original) {
      return(state)
    }
    return(state[setdiff(names(state), originals)])
  }

  workflow_check_name_collisions(state, entries, step_class)

  state <- c(state, entries)

  if (!keep_original) {
    state <- state[setdiff(names(state), originals)]
  }

  state
}

workflow_check_name_collisions <- function(state, entries, step_class) {
  collision <- intersect(names(state), names(entries))
  if (length(collision) > 0L) {
    workflow_stop_step(
      step_class,
      paste0(
        "Name collision occurred while compiling ", step_class,
        ": ", paste(collision, collapse = ", "), "."
      )
    )
  }
}

workflow_get_entry <- function(state, name, step_class) {
  entry <- state[[name]]
  if (is.null(entry)) {
    workflow_stop_step(
      step_class,
      paste0("Compiled recipe state does not contain required column `", name, "`.")
    )
  }
  entry
}

workflow_stop_step <- function(step_class, reason) {
  stop("Cannot compile `", step_class, "` in burgle.workflow(): ", reason)
}

workflow_poly_call <- function(x, object) {
  args <- list(
    x = x,
    degree = max(attr(object, "degree"))
  )

  coefs <- attr(object, "coefs")
  if (is.null(coefs)) {
    args$raw <- TRUE
  } else {
    args$coefs <- workflow_constant_call(coefs)
  }

  workflow_namespace_call("stats", "poly", args)
}

workflow_bs_call <- function(x, object) {
  args <- list(
    x = x,
    knots = workflow_constant_call(attr(object, "knots")),
    Boundary.knots = workflow_constant_call(attr(object, "Boundary.knots")),
    degree = attr(object, "degree"),
    intercept = isTRUE(attr(object, "intercept"))
  )

  workflow_namespace_call("splines", "bs", args)
}

workflow_ns_call <- function(x, object) {
  args <- list(
    x = x,
    knots = workflow_constant_call(attr(object, "knots")),
    Boundary.knots = workflow_constant_call(attr(object, "Boundary.knots")),
    intercept = isTRUE(attr(object, "intercept"))
  )

  workflow_namespace_call("splines", "ns", args)
}

workflow_splines2_call <- function(x, object) {
  args <- object
  args$.ns <- NULL
  args$.fn <- NULL
  args$nm <- NULL
  args$x <- x

  for (nm in names(args)) {
    if (!is.language(args[[nm]]) && !is.symbol(args[[nm]])) {
      args[[nm]] <- workflow_constant_call(args[[nm]])
    }
  }

  workflow_namespace_call("splines2", object$.fn, args)
}

workflow_matrix_column_call <- function(x, column) {
  as.call(list(as.name("["), x, quote(expr = ), column))
}

workflow_harmonic_call <- function(x, frequency, starting_val, cycle_size, fun) {
  cycle <- call(
    "*",
    2 * pi,
    call(
      "/",
      call("-", call("as.numeric", x), starting_val),
      cycle_size
    )
  )

  call(fun, call("*", cycle, frequency))
}

workflow_inverse_call <- function(x, step) {
  call("/", 1, call("+", x, step$offset))
}

workflow_invlogit_call <- function(x, step) {
  call("/", 1, call("+", 1, call("exp", call("-", x))))
}

workflow_logit_call <- function(x, step) {
  prepped <- call(
    "ifelse",
    call("==", x, 1),
    call("-", x, step$offset),
    call(
      "ifelse",
      call("==", x, 0),
      step$offset,
      x
    )
  )

  call(
    "log",
    call(
      "/",
      prepped,
      call("-", 1, prepped)
    )
  )
}

workflow_namespace_call <- function(pkg, fun, args) {
  fun_ref <- as.call(list(as.name("::"), as.name(pkg), as.name(fun)))
  as.call(c(list(fun_ref), args))
}

workflow_constant_call <- function(x) {
  x
}

workflow_terms_formula <- function(state, intercept) {
  exprs <- lapply(state, function(entry) workflow_term_expression(entry$expr, entry$scalar))

  rhs <- if (length(exprs) == 0L) {
    if (isTRUE(intercept)) 1 else 0
  } else {
    Reduce(function(x, y) call("+", x, y), exprs)
  }

  if (!isTRUE(intercept) && length(exprs) > 0L) {
    rhs <- call("+", 0, rhs)
  }

  formula <- call("~", rhs)
  class(formula) <- "formula"
  environment(formula) <- baseenv()
  formula
}

workflow_term_expression <- function(expr, scalar) {
  if (!isTRUE(scalar)) {
    return(expr)
  }

  if (is.symbol(expr)) {
    return(expr)
  }

  call("I", expr)
}

workflow_validate_design_matrices <- function(old_mm, new_mm, tolerance = 1e-8) {
  if (!identical(nrow(old_mm), nrow(new_mm))) {
    stop("Compiled workflow validation failed: old and new design matrices have different row counts.")
  }

  old_assign <- attr(old_mm, "assign")
  new_assign <- attr(new_mm, "assign")

  old_groups <- split(seq_len(ncol(old_mm)), old_assign)
  new_groups <- split(seq_len(ncol(new_mm)), new_assign)

  if (!identical(names(old_groups), names(new_groups))) {
    stop("Compiled workflow validation failed: old and new term groups do not align.")
  }

  old_order <- integer()
  new_order <- integer()

  for (group in names(old_groups)) {
    old_idx <- old_groups[[group]]
    new_idx <- new_groups[[group]]

    if (length(old_idx) != length(new_idx)) {
      stop("Compiled workflow validation failed for term group ", group, ": column counts differ.")
    }

    matched <- workflow_match_columns(old_mm[, old_idx, drop = FALSE], new_mm[, new_idx, drop = FALSE], tolerance = tolerance)
    old_order <- c(old_order, old_idx)
    new_order <- c(new_order, new_idx[matched])
  }

  old_reordered <- old_mm[, old_order, drop = FALSE]
  new_reordered <- new_mm[, new_order, drop = FALSE]

  if (!isTRUE(all.equal(unname(old_reordered), unname(new_reordered), tolerance = tolerance, check.attributes = FALSE))) {
    stop("Compiled workflow validation failed: the compiled raw-data design matrix is not numerically equivalent to the baked training design matrix.")
  }

  list(old_order = old_order, new_order = new_order)
}

workflow_match_columns <- function(old, new, tolerance = 1e-8) {
  used <- rep(FALSE, ncol(new))
  matched <- integer(ncol(old))

  for (i in seq_len(ncol(old))) {
    ok <- vapply(
      seq_len(ncol(new)),
      function(j) {
        !used[[j]] &&
          isTRUE(all.equal(
            unname(old[, i]),
            unname(new[, j]),
            tolerance = tolerance,
            check.attributes = FALSE
          ))
      },
      logical(1)
    )

    if (sum(ok) != 1L) {
      stop("Compiled workflow validation failed: could not uniquely align columns within a term group.")
    }

    j <- which(ok)
    matched[[i]] <- j
    used[[j]] <- TRUE
  }

  matched
}

workflow_check_runtime_dependencies <- function(object) {
  required_pkgs <- unique(object$workflow_required_pkgs)
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

  if (length(missing_pkgs) > 0L) {
    stop(
      "This burgled workflow requires runtime package(s) ",
      paste(sprintf("`%s`", missing_pkgs), collapse = ", "),
      " to evaluate compiled recipe terms during prediction."
    )
  }
}
