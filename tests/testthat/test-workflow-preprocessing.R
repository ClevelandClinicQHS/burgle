## ##############################################################################
## Compiled recipe steps: recipes::bake() is the numerical oracle
## ##############################################################################

recipe_compiler_data <- function() {
  data.frame(
    y = seq_len(40),
    x = seq(0.1, 0.9, length.out = 40),
    z = seq(1.1, 4.9, length.out = 40),
    f = factor(rep(c("a", "b", "c", "b"), 10)),
    ordered = ordered(rep(c("low", "mid", "high", "mid"), 10),
                      levels = c("low", "mid", "high"))
  )
}

recipe_compiler_step <- function(type, columns = "x", ...) {
  data <- recipe_compiler_data()
  rec <- recipes::recipe(y ~ ., data = data)
  if (type == "logit") {
    data$z <- seq(0.05, 0.95, length.out = nrow(data))
    rec <- recipes::recipe(y ~ ., data = data)
  }
  fun <- getExportedValue("recipes", paste0("step_", type))
  rec <- do.call(fun, c(list(rec), unname(lapply(columns, as.name)), list(...)))
  suppressWarnings(recipes::prep(rec, training = data))
}

expect_recipe_compiler_bake <- function(rec, data, tolerance = 1e-10) {
  compiled <- compile_workflow_recipe(rec)
  observed <- suppressWarnings(eval_workflow_recipe(compiled, data))
  if (!nrow(data)) {
    ## Older recipes versions cannot bake empty link or spline inputs.
    one <- as.data.frame(lapply(data, function(x) {
      if (is.factor(x)) factor(levels(x)[1L], levels = levels(x), ordered = is.ordered(x))
      else if (inherits(x, "Date")) as.Date("2020-01-01")
      else if (inherits(x, "POSIXct")) as.POSIXct("2020-01-01", tz = "UTC")
      else if (is.character(x)) "a"
      else 0.5
    }), check.names = FALSE)
    expected <- suppressWarnings(recipes::bake(rec, new_data = one))[0, ]
  } else {
    expected <- suppressWarnings(recipes::bake(rec, new_data = data))
  }
  expected <- as.data.frame(expected, check.names = FALSE)
  expect_identical(names(observed), names(expected))
  expect_equal(observed, expected, tolerance = tolerance, ignore_attr = FALSE)
  invisible(compiled)
}

test_that("scalar transforms preserve trained columns and link edge behavior", {
  skip_if_not_installed("recipes")
  configs <- list(
    log = list(base = 10, offset = 1),
    sqrt = list(),
    inverse = list(offset = 0.4),
    invlogit = list(),
    logit = list(offset = 0.01)
  )
  for (type in names(configs)) {
    rec <- do.call(recipe_compiler_step, c(list(type, c("x", "z")), configs[[type]]))
    data <- recipe_compiler_data()[c(7, 2, 20, 1), ]
    data$x <- c(0, 1, NA, 0.2)
    if (type == "logit") data$z <- c(1, 0, NA, 0.8)
    expect_recipe_compiler_bake(rec, data)
    expect_recipe_compiler_bake(rec, data[1, , drop = FALSE])
    expect_recipe_compiler_bake(rec, data[0, , drop = FALSE])
  }
  signed <- recipe_compiler_step("log", base = 2, signed = TRUE)
  data <- recipe_compiler_data()[1:7, ]
  data$x <- c(-8, -1, -0.2, 0, 0.2, 8, NA)
  expect_recipe_compiler_bake(signed, data)
  inv <- recipe_compiler_step("inverse", offset = 0)
  data$x <- c(0, -1, Inf, -Inf, NA, 2, 3)
  expect_recipe_compiler_bake(inv, data)
  link <- recipe_compiler_step("invlogit")
  expect_recipe_compiler_bake(link, data)
})

test_that("abs compiles a trained scalar step without retaining selectors", {
  skip_if_not_installed("recipes")
  rec <- recipes::prep(recipes::recipe(y ~ ., data = recipe_compiler_data()))
  ## Neither recipes nor extrasteps exports step_abs; support its trained schema.
  rec$steps <- list(structure(
    list(trained = TRUE, skip = FALSE, columns = c(x = "x")),
    class = c("step_abs", "step")
  ))
  data <- recipe_compiler_data()[1:4, ]
  data$x <- c(-1, 0, NA, -Inf)
  expected <- data
  expected$x <- abs(expected$x)
  expected <- expected[, rec$var_info$variable]
  expect_equal(eval_workflow_recipe(compile_workflow_recipe(rec), data), expected)
})

test_that("polynomial and base spline bases reuse trained parameters exactly", {
  skip_if_not_installed("recipes")
  configs <- list(
    list("poly", degree = 4),
    list("poly", degree = 3, options = list(raw = TRUE)),
    list("ns", deg_free = 4, options = list(intercept = TRUE)),
    list("ns", options = list(knots = c(0.3, 0.6), Boundary.knots = c(0, 1))),
    list("bs", deg_free = 5, degree = 2, options = list(intercept = TRUE)),
    list("bs", degree = 3, options = list(knots = c(0.3, 0.6),
                                         Boundary.knots = c(0, 1)))
  )
  for (config in configs) {
    for (keep in c(TRUE, FALSE)) {
      rec <- do.call(recipe_compiler_step, c(config, list(keep_original_cols = keep)))
      data <- recipe_compiler_data()[c(10, 1, 30, 2), ]
      data$x <- c(-0.2, 0.5, 1.2, NA)
      expect_recipe_compiler_bake(rec, data)
      expect_recipe_compiler_bake(rec, data[1, , drop = FALSE])
      expect_recipe_compiler_bake(rec, data[0, , drop = FALSE])
    }
  }
  rec <- recipe_compiler_step("poly", columns = c("z", "x"), degree = 2)
  expect_recipe_compiler_bake(rec, recipe_compiler_data()[2:5, ])
  rec <- recipe_compiler_step("ns", deg_free = 12)
  expect_recipe_compiler_bake(rec, recipe_compiler_data()[2:5, ])
  expect_true("x_ns_01" %in% compile_workflow_recipe(rec)$output$names)
})

test_that("all splines2 bases preserve full and reduced bases and boundaries", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("splines2")
  types <- c("spline_b", "spline_natural", "spline_monotone", "spline_convex",
             "spline_nonnegative", "poly_bernstein")
  data <- recipe_compiler_data()[c(30, 2, 15, 4), ]
  data$x <- c(0.1, 0.25, 0.9, NA)
  for (type in types) {
    if (!exists(paste0("step_", type), envir = asNamespace("recipes"))) next
    args <- if (type == "poly_bernstein") {
      list(degree = 4, options = list(Boundary.knots = c(0, 1)))
    } else {
      list(deg_free = 5, options = list(Boundary.knots = c(0, 1)))
    }
    for (complete in c(TRUE, FALSE)) {
      for (keep in c(TRUE, FALSE)) {
        rec <- do.call(recipe_compiler_step,
                       c(list(type), args, list(complete_set = complete,
                                               keep_original_cols = keep)))
        expect_recipe_compiler_bake(rec, data)
        expect_recipe_compiler_bake(rec, data[1, , drop = FALSE])
        compiled <- compile_workflow_recipe(rec)
        empty <- eval_workflow_recipe(compiled, data[0, , drop = FALSE])
        expect_equal(nrow(empty), 0L)
        expect_identical(names(empty), names(eval_workflow_recipe(compiled, data[1, ])))
      }
    }
  }
  rec <- recipe_compiler_step("spline_b", deg_free = 12, degree = 2,
                             options = list(periodic = TRUE))
  expect_recipe_compiler_bake(rec, data)
  rec <- recipe_compiler_step("spline_monotone", columns = c("z", "x"),
                             deg_free = 6, degree = 2,
                             options = list(knots = c(0.3, 0.6),
                                            Boundary.knots = c(0, 5)))
  expect_recipe_compiler_bake(rec, data)
})

test_that("harmonics preserve frequency order, phase, and date units", {
  skip_if_not_installed("recipes")
  rec <- recipe_compiler_step("harmonic", columns = c("z", "x"),
                             frequency = c(3, 1, 2, 1),
                             cycle_size = c(12, 2), starting_val = c(1, 0.2),
                             keep_original_cols = TRUE)
  data <- recipe_compiler_data()[c(9, 3, 4), ]
  data$x[2] <- NA
  expect_recipe_compiler_bake(rec, data)
  expect_recipe_compiler_bake(rec, data[1, , drop = FALSE])
  expect_recipe_compiler_bake(rec, data[0, , drop = FALSE])
  data <- data.frame(date = as.Date("2020-01-01") + seq_len(30))
  rec <- recipes::recipe(~ ., data = data) |>
    recipes::step_harmonic(date, frequency = c(1, 2), cycle_size = 365,
                           starting_val = as.Date("2020-01-01")) |>
    recipes::prep()
  expect_recipe_compiler_bake(rec, data[5:10, , drop = FALSE])
  data$date[] <- NA
  expect_error(eval_workflow_recipe(compile_workflow_recipe(rec), data),
               "at least one non-NA")
})

test_that("ratios resolve custom naming once and honor removal and denominator order", {
  skip_if_not_installed("recipes")
  for (keep in c(TRUE, FALSE)) {
    rec <- recipes::recipe(y ~ ., data = recipe_compiler_data()) |>
      recipes::step_ratio(x, z, denom = recipes::denom_vars(z, x),
                          keep_original_cols = keep,
                          naming = function(top, bottom) paste0(top, "__over__", bottom)) |>
      recipes::prep()
    data <- recipe_compiler_data()[1:4, ]
    data$x <- c(0, 2, NA, 3)
    data$z <- c(2, 0, 1, NA)
    compiled <- expect_recipe_compiler_bake(rec, data)
    expect_identical(compiled$ops[[1]]$outputs, c("x__over__z", "z__over__x"))
    expect_recipe_compiler_bake(rec, data[0, , drop = FALSE])
  }
})

test_that("lag is batch-local and retains factor and date column classes", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("dplyr")
  for (keep in c(TRUE, FALSE)) {
    rec <- recipe_compiler_step("lag", columns = c("x", "f"),
                               lag = c(2, 1, 0, 8), prefix = "previous_",
                               keep_original_cols = keep)
    data <- recipe_compiler_data()[c(30, 1, 20, 3), ]
    expect_recipe_compiler_bake(rec, data)
    expect_recipe_compiler_bake(rec, data[1, , drop = FALSE])
    expect_recipe_compiler_bake(rec, data[0, , drop = FALSE])
  }
  rec <- recipe_compiler_step("lag", lag = c(1, 3), default = -10)
  expect_recipe_compiler_bake(rec, recipe_compiler_data()[c(30, 1, 20), ])
  data <- data.frame(date = as.Date("2020-01-01") + seq_len(30))
  rec <- recipes::recipe(~ ., data = data) |>
    recipes::step_lag(date, lag = c(1, 2), default = as.Date("1999-01-01")) |>
    recipes::prep()
  expect_recipe_compiler_bake(rec, data[c(12, 3, 7), , drop = FALSE])
})

test_that("interactions preserve numeric, factor, ordered, and selector expansion", {
  skip_if_not_installed("recipes")
  for (keep in c(TRUE, FALSE)) {
    rec <- suppressWarnings(
      recipes::recipe(y ~ ., data = recipe_compiler_data()) |>
        recipes::step_interact(terms = ~ x:z + x:f + z:ordered + x:z:f,
                              sep = "__", keep_original_cols = keep) |>
        recipes::prep()
    )
    data <- recipe_compiler_data()[c(9, 2, 10, 1), ]
    data$x[2] <- NA
    expect_recipe_compiler_bake(rec, data)
    expect_recipe_compiler_bake(rec, data[1, , drop = FALSE])
    expect_recipe_compiler_bake(rec, data[0, , drop = FALSE])
  }
  rec <- recipes::recipe(y ~ x + z, data = recipe_compiler_data()) |>
    recipes::step_poly(x, degree = 2) |>
    recipes::step_interact(terms = ~ recipes::all_numeric_predictors():z) |>
    recipes::prep()
  expect_recipe_compiler_bake(rec, recipe_compiler_data()[c(5, 1, 30), c("y", "x", "z")])
  rec <- suppressWarnings(
    recipes::recipe(y ~ x + f, data = recipe_compiler_data()) |>
      recipes::step_lag(f, lag = 1) |>
      recipes::step_interact(terms = ~ x:lag_1_f) |>
      recipes::prep()
  )
  compiled <- expect_recipe_compiler_bake(rec, recipe_compiler_data()[1:4, c("y", "x", "f")])
  expect_identical(compiled$output$names,
                   names(eval_workflow_recipe(compiled, recipe_compiler_data()[1:4, c("y", "x", "f")])))
})

test_that("ordered operations, empty selectors, skipped steps, and metadata are compact", {
  skip_if_not_installed("recipes")
  rec <- recipes::recipe(y ~ x + z + f, data = recipe_compiler_data()) |>
    recipes::step_log(x, offset = 1) |>
    recipes::step_poly(x, degree = 2, keep_original_cols = TRUE) |>
    recipes::step_ratio(x_poly_1, denom = recipes::denom_vars(z),
                        keep_original_cols = TRUE) |>
    recipes::step_sqrt(z, skip = TRUE) |>
    recipes::prep()
  data <- recipe_compiler_data()[c(7, 1, 15), c("y", "x", "z", "f")]
  compiled <- expect_recipe_compiler_bake(rec, data)
  expect_identical(vapply(compiled$ops, `[[`, "", "type"), c("log", "poly", "ratio"))
  expect_identical(compiled$input$predictors, c("x", "z", "f"))
  expect_identical(compiled$output$predictors, setdiff(names(eval_workflow_recipe(compiled, data)), "y"))
  no_retained_state <- function(x) {
    if (is.environment(x) || is.function(x) || inherits(x, c("recipe", "workflow", "data.frame"))) {
      return(FALSE)
    }
    if (is.list(x)) return(all(vapply(x, no_retained_state, logical(1))))
    TRUE
  }
  expect_true(no_retained_state(compiled))
  expect_equal(unserialize(serialize(compiled, NULL)), compiled)
  for (type in c("poly", "ns", "bs", "sqrt", "log", "inverse", "invlogit", "logit", "lag")) {
    rec <- recipes::recipe(y ~ x, data = recipe_compiler_data())
    rec <- do.call(getExportedValue("recipes", paste0("step_", type)),
                   list(rec, quote(recipes::all_nominal_predictors())))
    rec <- recipes::prep(rec)
    expect_recipe_compiler_bake(rec, recipe_compiler_data()[1:3, c("y", "x")])
  }
  rec <- recipes::recipe(y ~ x, data = recipe_compiler_data()) |>
    recipes::step_center(x) |>
    recipes::prep()
  expect_warning(compiled <- compile_workflow_recipe(rec), "Unsupported recipe step 'step_center'")
  expect_equal(eval_workflow_recipe(compiled, data.frame(x = 1)), data.frame(x = 1))
  rec$steps[[1]]$skip <- TRUE
  expect_warning(compile_workflow_recipe(rec), NA)
})

test_that("compiler errors identify missing columns, collisions, and untrained steps", {
  skip_if_not_installed("recipes")
  rec <- recipe_compiler_step("poly")
  compiled <- compile_workflow_recipe(rec)
  expect_error(eval_workflow_recipe(compiled, data.frame(z = 1)),
               "Missing columns.*step_poly.*x")
  data <- recipe_compiler_data()[1, ]
  data$x_poly_1 <- 1
  expect_error(eval_workflow_recipe(compiled, data), "Name collision.*x_poly_1")
  expect_error(eval_workflow_recipe(compiled, matrix(1)), "data.frame")
  expect_error(compile_workflow_recipe(list()), "trained recipes recipe")
  rec <- recipes::recipe(y ~ ., data = recipe_compiler_data()) |>
    recipes::step_poly(x)
  expect_error(compile_workflow_recipe(rec), "not trained.*prep")
  expect_error(workflow_recipe_dependency("burgle.recipe.compiler.missing", "step_spline_b"),
               "step_spline_b.*install.packages")
})

test_that("prediction neither bakes recipes nor retains training-sized bases", {
  skip_if_not_installed("recipes")
  rec <- recipe_compiler_step("poly", degree = 3)
  data <- recipe_compiler_data()[1:4, ]
  expected <- as.data.frame(recipes::bake(rec, data))
  compiled <- compile_workflow_recipe(rec)
  testthat::local_mocked_bindings(
    bake = function(...) stop("Prediction must not call bake."),
    .package = "recipes"
  )
  expect_equal(eval_workflow_recipe(compiled, data), expected)
  expect_true(is.null(compiled$ops[[1]]$basis$x$x))
  expect_false(any(c("terms", "objects", "template", "recipe") %in% names(compiled)))
})
