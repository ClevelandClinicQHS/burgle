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

  extract_fit_engine <- workflow_namespace_function("workflows", "extract_fit_engine")
  extract_mold <- workflow_namespace_function("workflows", "extract_mold")
  extract_preprocessor <- workflow_namespace_function("workflows", "extract_preprocessor")
  extract_recipe <- workflow_namespace_function("workflows", "extract_recipe")

  engine <- extract_fit_engine(object)
  burgled <- workflow_burgle_engine(engine)

  preprocessor <- extract_preprocessor(object)

  if (inherits(preprocessor, "formula")) {
    stop(
      "burgle.workflow() does not support formula-preprocessor workflows because hardhat expands predictors before engine fitting and the raw terms needed for faithful reconstruction are not retained."
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

  if (!workflow_has_active_steps(recipe_trained)) {
    burgled$workflow_required_pkgs <- character()
    class(burgled) <- c("burgle_workflow", class(burgled))
    return(burgled)
  }

  compiled <- workflow_compile_recipe(
    recipe_trained = recipe_trained,
    recipe_untrained = recipe_untrained,
    baked_predictors = mold$predictors,
    burgled = burgled
  )

  out <- compiled$object
  out$workflow_required_pkgs <- compiled$required_pkgs

  class(out) <- c("burgle_workflow", class(out))
  out
}

#' @rdname predict_burgle
#'
#' @export
predict.burgle_workflow <- function(object, newdata, ...) {
  workflow_check_runtime_dependencies(object)
  class(object) <- setdiff(class(object), "burgle_workflow")
  stats::predict(object, newdata = newdata, ...)
}

workflow_require_namespace <- function(pkg, caller = "burgle.workflow()") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package `", pkg, "` must be installed to use ", caller, ".")
  }
}

workflow_namespace_function <- function(pkg, fun) {
  getExportedValue(pkg, fun)
}

workflow_has_active_steps <- function(recipe_trained) {
  if (length(recipe_trained$steps) == 0L) {
    return(FALSE)
  }

  any(!vapply(recipe_trained$steps, function(x) isTRUE(x$skip), logical(1)))
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

workflow_terms_intercept <- function(terms) {
  isTRUE(attr(terms, "intercept") == 1L)
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

workflow_compile_recipe <- function(recipe_trained, recipe_untrained, baked_predictors, burgled) {
  raw_training <- recipe_untrained$template
  if (is.null(raw_training) || !is.data.frame(raw_training) || nrow(raw_training) == 0L) {
    stop(
      "burgle.workflow() needs the original recipe template rows to validate the compiled terms against the baked training design matrix."
    )
  }

  original_info <- summary(recipe_untrained, original = TRUE)
  predictor_names <- unique(original_info$variable[original_info$role == "predictor"])
  state <- workflow_initial_state(raw_training, predictor_names)
  workflow_validate_baked_predictors(baked_predictors)
  raw_training_used <- workflow_training_rows(raw_training, baked_predictors)

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

  if (!workflow_supports_compiled_terms(burgled)) {
    if (workflow_has_active_steps(recipe_trained)) {
      stop(
        "burgle.workflow() does not support recipe preprocessing for burgled objects of class `",
        class(burgled)[1],
        "` because their prediction path does not use a replaceable formula/terms design matrix."
      )
    }

    return(list(
      object = burgled,
      required_pkgs = unique(required_pkgs)
    ))
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
    intercept = workflow_terms_intercept(burgled$terms)
  )

  compiled_terms <- stats::terms(compiled_formula, data = raw_training_used)

  mf <- stats::model.frame(compiled_formula, data = raw_training_used, na.action = stats::na.pass)
  xlevels <- stats::.getXlevels(compiled_terms, mf)

  contrasts <- burgled$contrasts
  if (length(contrasts) > 0L) {
    contrasts <- contrasts[intersect(names(contrasts), names(xlevels))]
  }

  old_mm <- workflow_model_matrix(
    burgled,
    burgled$terms,
    data = baked_predictors,
    xlev = burgled$xlevels,
    contrasts.arg = burgled$contrasts
  )
  new_mm <- workflow_model_matrix(
    burgled,
    compiled_terms,
    data = raw_training_used,
    xlev = xlevels,
    contrasts.arg = contrasts
  )

  column_map <- workflow_validate_design_matrices(
    old_mm,
    new_mm,
    old_terms = burgled$terms,
    new_terms = compiled_terms
  )

  burgled <- workflow_align_burgled_object(
    burgled = burgled,
    old_mm = old_mm,
    new_mm = new_mm,
    column_map = column_map
  )

  burgled$terms <- compiled_terms
  burgled$xlevels <- xlevels
  burgled$contrasts <- contrasts

  list(
    object = burgled,
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
  arg_names <- switch(
    object$.fn,
    bSpline = c("df", "degree", "knots", "Boundary.knots", "intercept", "complete_basis", "periodic", "derivs", "integral", "warn.outside"),
    naturalSpline = c("df", "knots", "Boundary.knots", "intercept", "complete_basis", "derivs", "integral", "warn.outside"),
    iSpline = c("df", "degree", "knots", "Boundary.knots", "intercept", "complete_basis", "derivs", "integral", "warn.outside"),
    cSpline = c("df", "degree", "knots", "Boundary.knots", "intercept", "complete_basis", "derivs", "integral", "scale", "warn.outside"),
    mSpline = c("df", "degree", "knots", "Boundary.knots", "intercept", "complete_basis", "periodic", "derivs", "integral", "warn.outside"),
    bernsteinPoly = c("degree", "intercept", "complete_basis", "derivs", "integral"),
    character()
  )
  arg_names <- intersect(arg_names, names(object))
  args <- object[arg_names]
  args$x <- x

  for (nm in names(args)) {
    if (!is.language(args[[nm]]) && !is.symbol(args[[nm]])) {
      args[[nm]] <- workflow_constant_call(args[[nm]])
    }
  }

  workflow_namespace_call("splines2", object$.fn, args)
}

workflow_matrix_column_call <- function(x, column) {
  substitute(X[, J], list(X = x, J = column))
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

workflow_validate_design_matrices <- function(old_mm, new_mm, old_terms = NULL, new_terms = NULL, tolerance = 1e-8) {
  if (!identical(nrow(old_mm), nrow(new_mm))) {
    stop("Compiled workflow validation failed: old and new design matrices have different row counts.")
  }

  old_groups <- workflow_design_groups(old_mm, old_terms)
  new_groups <- workflow_design_groups(new_mm, new_terms)

  if (!identical(names(old_groups), names(new_groups))) {
    if (length(old_groups) != length(new_groups)) {
      stop("Compiled workflow validation failed: old and new term groups do not align.")
    }
    names(old_groups) <- as.character(seq_along(old_groups))
    names(new_groups) <- as.character(seq_along(new_groups))
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

workflow_design_groups <- function(mm, terms) {
  if (ncol(mm) == 0L) {
    return(list())
  }

  assign <- attr(mm, "assign")
  groups <- split(seq_len(ncol(mm)), assign)

  if (is.null(terms)) {
    return(groups)
  }

  labels <- attr(terms, "term.labels")
  names(groups) <- vapply(
    names(groups),
    function(x) {
      idx <- as.integer(x)
      if (is.na(idx) || idx == 0L) {
        "(Intercept)"
      } else if (idx > length(labels)) {
        as.character(idx)
      } else {
        unname(labels[idx])
      }
    },
    character(1)
  )

  groups
}

workflow_check_runtime_dependencies <- function(object) {
  required_pkgs <- object$workflow_required_pkgs
  if (is.null(required_pkgs)) {
    required_pkgs <- character()
  }
  required_pkgs <- unique(required_pkgs)
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

  if (length(missing_pkgs) > 0L) {
    stop(
      "This burgled workflow requires runtime package(s) ",
      paste(sprintf("`%s`", missing_pkgs), collapse = ", "),
      " to evaluate compiled recipe terms during prediction."
    )
  }
}

workflow_validate_baked_predictors <- function(baked_predictors) {
  bad_columns <- names(baked_predictors)[vapply(
    baked_predictors,
    function(x) is.matrix(x) || is.array(x) || is.list(x),
    logical(1)
  )]

  if (length(bad_columns) > 0L) {
    stop(
      "burgle.workflow() does not support workflows whose baked predictors contain matrix or list columns, including sparse or multi-column recipe outputs such as `",
      bad_columns[[1]],
      "`."
    )
  }
}

workflow_training_rows <- function(raw_training, baked_predictors) {
  raw_rows <- rownames(raw_training)
  baked_rows <- rownames(baked_predictors)
  default_raw_rows <- identical(raw_rows, as.character(seq_len(nrow(raw_training))))
  training_rows <- suppressWarnings(as.integer(baked_rows))

  if (!is.null(raw_rows) && !is.null(baked_rows) && all(baked_rows %in% raw_rows)) {
    if (anyDuplicated(raw_rows) || anyDuplicated(baked_rows)) {
      stop(
        "burgle.workflow() could not determine a one-to-one mapping between raw training rows and the rows that reached the fitted engine after recipe preprocessing."
      )
    }
    return(raw_training[match(baked_rows, raw_rows), , drop = FALSE])
  }

  if (default_raw_rows &&
      length(training_rows) == nrow(baked_predictors) &&
      !anyNA(training_rows)) {
    return(raw_training[training_rows, , drop = FALSE])
  }

  if (!is.null(raw_rows) && !is.null(baked_rows) &&
      identical(baked_rows, raw_rows[seq_len(length(baked_rows))])) {
    return(raw_training[seq_len(nrow(baked_predictors)), , drop = FALSE])
  }

  stop(
    "burgle.workflow() could not determine which training rows reached the fitted engine after recipe preprocessing."
  )
}

workflow_model_matrix <- function(object, terms, data, xlev, contrasts.arg) {
  mm <- stats::model.matrix(
    terms,
    data = data,
    xlev = xlev,
    contrasts.arg = contrasts.arg
  )

  if (inherits(object, c("burgle_coxph", "burgle_cph", "burgle_flexsurvreg")) &&
      workflow_terms_intercept(terms)) {
    keep <- attr(mm, "assign") != 0L
    mm <- mm[, keep, drop = FALSE]
    attr(mm, "assign") <- attr(mm, "assign")[keep]
  }

  mm
}

workflow_align_burgled_object <- function(burgled, old_mm, new_mm, column_map) {
  if (inherits(burgled, "burgle_multinom")) {
    return(workflow_align_multinom_object(burgled, old_mm, new_mm, column_map))
  }

  if (inherits(burgled, "burgle_flexsurvreg")) {
    return(workflow_align_flexsurvreg_object(burgled, old_mm, new_mm, column_map))
  }

  return(workflow_align_simple_object(burgled, old_mm, new_mm, column_map))
}

workflow_align_simple_object <- function(object, old_mm, new_mm, column_map) {
  coef <- object$coef
  if (anyNA(coef) || !all(colnames(old_mm) %in% names(coef))) {
    stop(
      "burgle.workflow() does not support rank-deficient or aliased workflow fits because the fitted coefficients are not fully aligned with the baked design matrix columns."
    )
  }
  coef <- coef[colnames(old_mm)]

  cov <- object$cov
  if (!all(colnames(old_mm) %in% rownames(cov)) || !all(colnames(old_mm) %in% colnames(cov))) {
    stop(
      "burgle.workflow() does not support rank-deficient or aliased workflow fits because the fitted covariance matrix is not fully aligned with the baked design matrix columns."
    )
  }
  cov <- cov[colnames(old_mm), colnames(old_mm), drop = FALSE]

  coef_new <- unname(coef[column_map$old_order])
  names(coef_new) <- colnames(new_mm)[column_map$new_order]

  cov_new <- cov[column_map$old_order, column_map$old_order, drop = FALSE]
  rownames(cov_new) <- colnames(new_mm)[column_map$new_order]
  colnames(cov_new) <- colnames(new_mm)[column_map$new_order]

  object$coef <- coef_new
  object$cov <- cov_new
  object
}

workflow_align_flexsurvreg_object <- function(object, old_mm, new_mm, column_map) {
  parameter_indices <- flexsurv_pars_indices(object)
  if (is.null(parameter_indices)) {
    stop(
      "burgle.workflow() could not identify the distribution-parameter indices for the burgled flexsurv workflow."
    )
  }

  covariate_indices <- setdiff(seq_along(object$coef), parameter_indices)

  if (length(covariate_indices) != ncol(old_mm)) {
    stop(
      "burgle.workflow() could not align flexsurv workflow coefficients with the baked design matrix columns."
    )
  }

  coef <- object$coef
  cov <- object$cov

  covariate_names <- names(coef)[covariate_indices]
  if (anyNA(coef[covariate_indices]) || !identical(unname(covariate_names), unname(colnames(old_mm)))) {
    stop(
      "burgle.workflow() does not support rank-deficient or nonstandard flexsurv workflow coefficient layouts."
    )
  }

  perm <- seq_along(coef)
  perm[covariate_indices] <- covariate_indices[column_map$old_order]
  coef <- coef[perm]
  names(coef)[covariate_indices] <- colnames(new_mm)[column_map$new_order]
  cov <- cov[perm, perm, drop = FALSE]
  rownames(cov) <- names(coef)
  colnames(cov) <- names(coef)

  object$coef <- coef
  object$cov <- cov
  object
}

workflow_align_multinom_object <- function(object, old_mm, new_mm, column_map) {
  rnl <- length(object$rlev) - 1L
  p <- ncol(old_mm)

  if (length(object$coef) != rnl * p) {
    stop(
      "burgle.workflow() could not align multinom workflow coefficients with the baked design matrix columns."
    )
  }

  coef <- object$coef
  cov <- object$cov
  perm <- integer(length(coef))
  block_names <- colnames(new_mm)[column_map$new_order]
  new_names <- character(length(coef))

  for (i in seq_len(rnl)) {
    block <- ((i - 1L) * p + 1L):(i * p)
    perm[block] <- block[column_map$old_order]
    prefix <- paste0(object$rlev[[i + 1L]], ":")
    new_names[block] <- paste0(prefix, block_names)
  }

  coef <- unname(coef[perm])
  names(coef) <- new_names
  cov <- cov[perm, perm, drop = FALSE]
  rownames(cov) <- new_names
  colnames(cov) <- new_names

  object$coef <- coef
  object$cov <- cov
  object
}
