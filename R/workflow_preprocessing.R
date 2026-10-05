workflow_recipe_columns <- function(columns) {
  if (!is.null(names(columns))) names(columns) else as.character(columns)
}

workflow_recipe_names <- function(n, prefix) {
  if (n == 0L) return(character())
  paste0(prefix, formatC(seq_len(n), width = nchar(as.character(n)), flag = "0"))
}

workflow_recipe_dependency <- function(package, step) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Recipe step '", step, "' requires package '", package,
         "'. Install it with install.packages(\"", package, "\").", call. = FALSE)
  }
}

workflow_recipe_levels <- function(levels, template = NULL) {
  out <- list()
  for (column in names(levels)) {
    item <- levels[[column]]
    if (is.null(item$factor) || all(is.na(item$values))) next
    if (!is.null(template) && !is.factor(template[[column]])) next
    out[[column]] <- list(values = item$values, ordered = isTRUE(item$ordered))
  }
  out
}

## Retain parameters, not the fitted basis matrix or any training observations.
workflow_recipe_basis <- function(object, kind) {
  if (kind == "poly") {
    return(list(degree = attr(object, "degree"), coefs = attr(object, "coefs"),
                size = ncol(object)))
  }
  list(degree = attr(object, "degree"), knots = attr(object, "knots"),
       Boundary.knots = attr(object, "Boundary.knots"),
       intercept = attr(object, "intercept"), size = ncol(object))
}

compile_workflow_recipe <- function(recipe) {
  if (!inherits(recipe, "recipe")) {
    stop("Expected a trained recipes recipe.", call. = FALSE)
  }
  supported <- c("poly", "ns", "bs", "interact", "spline_b", "spline_natural",
                 "harmonic", "poly_bernstein", "spline_monotone", "spline_convex",
                 "spline_nonnegative", "log", "sqrt", "inverse", "invlogit",
                 "logit", "abs", "ratio", "lag")
  info <- recipe$var_info
  input_names <- unique(info$variable)
  current <- input_names
  roles <- setNames(as.character(info$role), info$variable)
  input_levels <- workflow_recipe_levels(recipe$orig_lvls)
  for (column in names(input_levels)) {
    if (!isTRUE(recipe$orig_lvls[[column]]$factor) &&
        (is.null(recipe$levels) || !any(roles[names(roles) == column] %in% c("predictor", "outcome")))) {
      input_levels[[column]] <- NULL
    }
  }
  prototype <- recipe$ptype
  if (is.null(prototype)) prototype <- list()
  for (column in input_names) {
    if (!is.null(prototype[[column]])) next
    nominal <- recipe$orig_lvls[[column]]
    prototype[[column]] <- if (!is.null(nominal$factor)) {
      factor(character(), levels = nominal$values, ordered = isTRUE(nominal$ordered))
    } else numeric()
  }
  ops <- list()

  for (position in seq_along(recipe$steps)) {
    step <- recipe$steps[[position]]
    type <- sub("^step_", "", class(step)[1L])
    if (!type %in% supported) {
      warning("Unsupported recipe step '", class(step)[1L],
              "' at position ", position, " (id: ",
              if (is.null(step$id)) "<none>" else step$id,
              ") is being omitted",
              if (isTRUE(step$skip)) " (skip = TRUE)" else "",
              "; predictions may differ from the workflow.",
              call. = FALSE)
      next
    }
    if (isTRUE(step$skip)) next
    if (!isTRUE(step$trained)) {
      stop("Recipe step '", class(step)[1L],
           "' is not trained. Fit the workflow or prep() the recipe first.",
           call. = FALSE)
    }
    op <- list(type = type, position = position,
               id = if (is.null(step$id)) "<none>" else step$id,
               inputs = character(), outputs = character(), remove = character())
    if (type %in% c("poly", "ns", "bs")) {
      op$inputs <- names(step$objects)
      op$basis <- lapply(step$objects, workflow_recipe_basis, kind = type)
      op$names <- lapply(op$inputs, function(column) {
        size <- op$basis[[column]]$size
        if (type == "poly") paste(column, "poly", seq_len(size), sep = "_")
        else workflow_recipe_names(size, paste0(column, "_", type, "_"))
      })
      names(op$names) <- op$inputs
      op$outputs <- unlist(op$names, use.names = FALSE)
    } else if (type %in% c("spline_b", "spline_natural", "poly_bernstein",
                           "spline_monotone", "spline_convex", "spline_nonnegative")) {
      workflow_recipe_dependency("splines2", paste0("step_", type))
      op$inputs <- names(step$results)
      op$basis <- lapply(step$results, function(result) {
        ## splines2 stores dimensions as metadata; x is never needed to predict.
        result$size <- result$dim[2L]
        result$dim <- NULL
        result$x <- NULL
        result
      })
      op$names <- lapply(op$inputs, function(column) {
        workflow_recipe_names(op$basis[[column]]$size, paste0(column, "_"))
      })
      names(op$names) <- op$inputs
      op$outputs <- unlist(op$names, use.names = FALSE)
    } else if (type == "harmonic") {
      op$inputs <- names(step$starting_val)
      op$frequency <- unname(step$frequency)
      op$starting_val <- step$starting_val
      op$cycle_size <- step$cycle_size
      op$names <- lapply(op$inputs, function(column) {
        paste0(column, rep(c("_sin_", "_cos_"), each = length(op$frequency)),
               seq_along(op$frequency))
      })
      names(op$names) <- op$inputs
      op$outputs <- unlist(op$names, use.names = FALSE)
    } else if (type == "ratio") {
      op$top <- as.character(step$columns$top)
      op$bottom <- as.character(step$columns$bottom)
      op$inputs <- unique(c(op$top, op$bottom))
      op$outputs <- vapply(seq_along(op$top), function(i) {
        step$naming(op$top[i], op$bottom[i])
      }, character(1))
    } else if (type == "lag") {
      workflow_recipe_dependency("dplyr", "step_lag")
      op$inputs <- workflow_recipe_columns(step$columns)
      op$lag <- step$lag
      op$default <- step$default
      op$prefix <- step$prefix
      op$outputs <- unlist(lapply(op$inputs, function(column) {
        paste0(op$prefix, op$lag, "_", column)
      }), use.names = FALSE)
    } else if (type == "interact") {
      objects <- step$objects
      if (length(objects) && !all(is.na(objects))) {
        op$formulas <- lapply(objects, function(object) {
          paste(deparse(stats::formula(object), width.cutoff = 500L), collapse = "")
        })
        op$inputs <- unique(unlist(lapply(objects, all.vars), use.names = FALSE))
        op$sep <- step$sep
        ## Construct a single synthetic row solely to resolve factor expansion.
        interaction_inputs <- union(current, op$inputs)
        fake <- setNames(lapply(interaction_inputs, function(column) {
          value <- prototype[[column]]
          if (is.null(value)) {
            ## Omitted producers may supply an interaction input externally.
            ## Read only its trained type/levels, never its training values.
            value <- recipe$template[[column]][0]
            nominal <- recipe$levels[[column]]
            if (!is.null(nominal$factor)) {
              value <- factor(character(), levels = nominal$values,
                              ordered = isTRUE(nominal$ordered))
            }
          }
          if (is.factor(value)) {
            ans <- factor(levels(value)[1L], levels = levels(value),
                          ordered = is.ordered(value))
            attr(ans, "contrasts") <- attr(value, "contrasts")
            ans
          } else 0
        }), interaction_inputs)
        fake <- as.data.frame(fake, check.names = FALSE)
        op$contrasts <- list()
        op$names <- list()
        for (i in seq_along(op$formulas)) {
          formula <- stats::as.formula(op$formulas[[i]], env = baseenv())
          contrasts <- attr(objects[[i]], "contrasts")
          matrix <- stats::model.matrix(formula, fake, contrasts.arg = contrasts)
          contrasts <- attr(matrix, "contrasts")
          if (length(contrasts)) {
            for (column in names(contrasts)) {
              value <- fake[[column]]
              stats::contrasts(value) <- contrasts[[column]]
              contrasts[[column]] <- stats::contrasts(value)
            }
          }
          op$contrasts[i] <- list(contrasts)
          op$names[[i]] <- gsub(":", op$sep, colnames(matrix)[grepl(":", colnames(matrix))])
        }
        op$outputs <- unlist(op$names, use.names = FALSE)
      } else {
        op$formulas <- list()
      }
    } else {
      op$inputs <- workflow_recipe_columns(step$columns)
      if (type == "log") {
        op$base <- step$base
        op$offset <- if (is.null(step$offset)) 0 else step$offset
        op$signed <- isTRUE(step$signed)
      }
      if (type %in% c("inverse", "logit")) op$offset <- step$offset
    }
    keep <- if (is.null(step$keep_original_cols)) {
      ## Before recipes 1.0.7 these two steps always retained their inputs.
      type %in% c("interact", "lag")
    } else isTRUE(step$keep_original_cols)
    if (length(op$outputs) && !keep) op$remove <- op$inputs
    for (i in seq_along(op$outputs)) {
      prototype[[op$outputs[i]]] <- if (type == "lag") {
        prototype[[op$inputs[ceiling(i / length(op$lag))]]]
      } else numeric()
    }
    prototype <- prototype[!names(prototype) %in% op$remove]
    current <- c(setdiff(current, op$remove), op$outputs)
    roles <- roles[!names(roles) %in% op$remove]
    if (length(op$outputs)) {
      roles <- c(roles, setNames(rep(step$role, length(op$outputs)), op$outputs))
    }
    ops[[length(ops) + 1L]] <- op
  }
  list(
    ops = ops,
    input = list(names = input_names,
                 predictors = unique(info$variable[info$role %in% "predictor"]),
                 levels = input_levels),
    output = list(names = current, predictors = names(roles)[roles %in% "predictor"],
                  levels = workflow_recipe_levels(recipe$levels, recipe$template))
  )
}

workflow_recipe_append <- function(data, columns, op) {
  new_names <- names(columns)
  collision <- intersect(names(data), new_names)
  if (length(collision)) {
    stop("Name collision in recipe step 'step_", op$type, "': ",
         paste(collision, collapse = ", "), ".", call. = FALSE)
  }
  if (anyDuplicated(new_names)) {
    stop("Duplicated output names in recipe step 'step_", op$type, "'.", call. = FALSE)
  }
  for (column in new_names) data[[column]] <- columns[[column]]
  data[, !names(data) %in% op$remove, drop = FALSE]
}

workflow_recipe_normalize_levels <- function(data, levels) {
  for (column in intersect(names(levels), names(data))) {
    info <- levels[[column]]
    values <- as.character(data[[column]])
    novel <- unique(values[!is.na(values) & !values %in% info$values])
    if (length(novel)) {
      stop("Unknown factor levels in column '", column, "': ",
           paste(novel, collapse = ", "),
           ". Supply levels observed in training.", call. = FALSE)
    }
    data[[column]] <- factor(values, levels = info$values,
                             ordered = info$ordered, exclude = NULL)
  }
  data
}

eval_workflow_recipe <- function(compiled, newdata) {
  if (!is.data.frame(newdata)) {
    stop("'newdata' must be a data.frame.", call. = FALSE)
  }
  data <- as.data.frame(newdata, check.names = FALSE)
  rownames(data) <- NULL
  ## Recipe operations follow training-column order, including untouched inputs.
  order <- c(intersect(compiled$input$names, names(data)),
             setdiff(names(data), compiled$input$names))
  data <- data[, order, drop = FALSE]
  data <- workflow_recipe_normalize_levels(data, compiled$input$levels)
  for (op in compiled$ops) {
    missing <- setdiff(op$inputs, names(data))
    if (length(missing)) {
      stop("Missing columns for recipe step 'step_", op$type, "': ",
           paste(missing, collapse = ", "), " (position ", op$position,
           ", id: ", op$id, "). Supply the raw or externally preprocessed ",
           "columns; an earlier unsupported or skip = TRUE step may have been omitted.",
           call. = FALSE)
    }
    type <- op$type
    columns <- list()
    if (type %in% c("poly", "ns", "bs")) {
      for (column in op$inputs) {
        basis <- op$basis[[column]]
        x <- data[[column]]
        if (!length(x)) {
          values <- matrix(numeric(), nrow = 0L, ncol = basis$size)
        } else if (type == "poly") {
          values <- stats::poly(x, degree = max(basis$degree),
                                coefs = basis$coefs, raw = is.null(basis$coefs),
                                simple = TRUE)
        } else {
          args <- basis[c("knots", "Boundary.knots", "intercept")]
          if (type == "bs") args$degree <- basis$degree
          args$x <- unique(x)
          values <- do.call(if (type == "bs") splines::bs else splines::ns, args)
          values <- values[match(x, args$x), , drop = FALSE]
        }
        for (i in seq_along(op$names[[column]])) {
          columns[[op$names[[column]][i]]] <- values[, i]
        }
      }
    } else if (type %in% c("spline_b", "spline_natural", "poly_bernstein",
                           "spline_monotone", "spline_convex", "spline_nonnegative")) {
      workflow_recipe_dependency("splines2", paste0("step_", type))
      for (column in op$inputs) {
        args <- op$basis[[column]]
        fun <- getExportedValue("splines2", args$.fn)
        args[c(".fn", ".ns", "nm", "size")] <- NULL
        x <- data[[column]]
        if (!length(x)) x <- args$Boundary.knots[1L]
        args$x <- x
        values <- do.call(fun, args)
        if (!nrow(data)) values <- values[0, , drop = FALSE]
        for (i in seq_along(op$names[[column]])) {
          columns[[op$names[[column]][i]]] <- values[, i]
        }
      }
    } else if (type == "harmonic") {
      for (column in op$inputs) {
        x <- as.numeric(data[[column]])
        if (length(x) && all(is.na(x))) {
          stop("Variable must have at least one non-NA value.", call. = FALSE)
        }
        cycle <- 2 * (pi * ((x - op$starting_val[column]) / op$cycle_size[column]))
        values <- c(lapply(op$frequency, function(f) sin(cycle * f)),
                    lapply(op$frequency, function(f) cos(cycle * f)))
        names(values) <- op$names[[column]]
        columns <- c(columns, values)
      }
    } else if (type == "ratio") {
      for (i in seq_along(op$top)) {
        columns[[op$outputs[i]]] <- data[[op$top[i]]] / data[[op$bottom[i]]]
      }
    } else if (type == "lag") {
      workflow_recipe_dependency("dplyr", "step_lag")
      for (column in op$inputs) {
        for (i in seq_along(op$lag)) {
          name <- paste0(op$prefix, op$lag[i], "_", column)
          columns[[name]] <- dplyr::lag(data[[column]], op$lag[i], default = op$default)
        }
      }
    } else if (type == "interact") {
      for (i in seq_along(op$formulas)) {
        formula <- stats::as.formula(op$formulas[[i]], env = baseenv())
        frame <- stats::model.frame(formula, data, na.action = stats::na.pass)
        matrix <- stats::model.matrix(formula, frame, contrasts.arg = op$contrasts[[i]])
        matrix <- matrix[, grepl(":", colnames(matrix)), drop = FALSE]
        new_names <- gsub(":", op$sep, colnames(matrix))
        for (j in seq_along(new_names)) columns[[new_names[j]]] <- matrix[, j]
      }
    } else {
      if (type == "log" && op$signed && op$offset != 0) {
        warning("When 'signed' is TRUE, 'offset' will be ignored.", call. = FALSE)
      }
      for (column in op$inputs) {
        x <- data[[column]]
        data[[column]] <- switch(type,
          log = if (op$signed) {
            ifelse(abs(x) < 1, 0, sign(x) * log(abs(x), base = op$base))
          } else log(x + op$offset, base = op$base),
          sqrt = sqrt(x),
          inverse = 1 / (x + op$offset),
          invlogit = if (length(x)) stats::binomial()$linkinv(x) else x,
          logit = if (length(x)) {
            x <- ifelse(x == 1, x - op$offset, x)
            x <- ifelse(x == 0, op$offset, x)
            stats::binomial()$linkfun(x)
          } else x,
          abs = abs(x)
        )
      }
      next
    }
    data <- workflow_recipe_append(data, columns, op)
  }
  workflow_recipe_normalize_levels(data, compiled$output$levels)
}
