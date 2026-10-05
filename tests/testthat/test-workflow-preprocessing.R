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

test_that("burgle-owned abs step matches bake and preserves base abs semantics", {
  skip_if_not_installed("recipes")
  rec <- recipes::recipe(y ~ ., data = recipe_compiler_data()) |>
    step_abs(x, z) |>
    recipes::prep()
  data <- recipe_compiler_data()[1:7, ]
  data$x <- c(-1, 0, NA, -Inf, Inf, NaN, -0)
  data$z <- c(-2L, 0L, NA_integer_, 1L, -3L, 4L, 5L)
  expect_recipe_compiler_bake(rec, data)
  expect_recipe_compiler_bake(rec, data[1, , drop = FALSE])
  expect_recipe_compiler_bake(rec, data[0, , drop = FALSE])
  observed <- eval_workflow_recipe(compile_workflow_recipe(rec), data)
  expect_identical(observed$x, abs(data$x))
  expect_identical(observed$z, abs(data$z))
  expect_equal(recipes::tidy(rec, number = 1)$terms, c("x", "z"))
  expect_invisible(print(rec$steps[[1]]))
  expect_error(
    recipes::prep(step_abs(recipes::recipe(y ~ ., data = recipe_compiler_data()), f)),
    "factor|numeric|double|integer"
  )
  skipped <- recipes::recipe(y ~ x, data = recipe_compiler_data()) |>
    step_abs(x, skip = TRUE) |>
    recipes::prep()
  expect_recipe_compiler_bake(skipped, data[, c("y", "x")])
  empty <- recipes::recipe(y ~ x, data = recipe_compiler_data()) |>
    step_abs(recipes::all_nominal_predictors()) |>
    recipes::prep()
  expect_recipe_compiler_bake(empty, data[, c("y", "x")])
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
  expect_identical(compiled$ops, list())
  expect_warning(compile_workflow_recipe(rec), paste0("position 1.*", rec$steps[[1]]$id))
  expect_equal(eval_workflow_recipe(compiled, data.frame(x = 1)), data.frame(x = 1))
  rec$steps[[1]]$skip <- TRUE
  expect_warning(compile_workflow_recipe(rec), "position 1.*skip = TRUE")
  rec <- recipe_compiler_step("poly")
  rec$steps[[1]]$skip <- TRUE
  expect_warning(compile_workflow_recipe(rec), NA)
})

test_that("legacy lag and interaction steps retain their original columns", {
  skip_if_not_installed("recipes")
  rec <- recipe_compiler_step("lag", lag = c(1, 2))
  data <- recipe_compiler_data()[1:4, ]
  expected <- as.data.frame(recipes::bake(rec, data))
  rec$steps[[1]]$keep_original_cols <- NULL
  expect_equal(eval_workflow_recipe(compile_workflow_recipe(rec), data), expected)
  rec <- recipes::recipe(y ~ x + z, data = recipe_compiler_data()) |>
    recipes::step_interact(terms = ~ x:z) |>
    recipes::prep()
  data <- data[, c("y", "x", "z")]
  expected <- as.data.frame(recipes::bake(rec, data))
  rec$steps[[1]]$keep_original_cols <- NULL
  expect_equal(eval_workflow_recipe(compile_workflow_recipe(rec), data), expected)
})

test_that("captured interaction contrast matrices survive changed global options", {
  skip_if_not_installed("recipes")
  rec <- suppressWarnings(
    recipes::recipe(y ~ x + f + ordered, data = recipe_compiler_data()) |>
      recipes::step_interact(terms = ~ x:f + x:ordered) |>
      recipes::prep()
  )
  data <- recipe_compiler_data()[1:4, c("y", "x", "f", "ordered")]
  expected <- as.data.frame(recipes::bake(rec, data))
  compiled <- compile_workflow_recipe(rec)
  expect_true(all(vapply(compiled$ops[[1]]$contrasts[[1]], is.matrix, logical(1))))
  old <- getOption("contrasts")
  options(contrasts = c("contr.sum", "contr.helmert"))
  tryCatch(
    expect_equal(eval_workflow_recipe(compiled, data), expected),
    finally = options(contrasts = old)
  )
})

test_that("skipped producers require externally supplied inputs only when consumed", {
  skip_if_not_installed("recipes")
  rec <- recipes::recipe(y ~ x, data = recipe_compiler_data()) |>
    recipes::step_poly(x, skip = TRUE) |>
    recipes::step_log(x_poly_1, offset = 10) |>
    recipes::step_lag(x_poly_1, skip = TRUE) |>
    recipes::prep()
  compiled <- compile_workflow_recipe(rec)
  expect_identical(vapply(compiled$ops, `[[`, "", "type"), "log")
  expect_error(eval_workflow_recipe(compiled, data.frame(x = 0.5)),
               "position 2.*Supply.*externally.*skip = TRUE")
  expect_equal(eval_workflow_recipe(compiled, data.frame(x = 0.5, x_poly_1 = 1)),
               data.frame(x = 0.5, x_poly_1 = log(11)))
})

test_that("interactions compile with externally supplied omitted-producer inputs", {
  skip_if_not_installed("recipes")
  data <- recipe_compiler_data()
  rec <- recipes::recipe(y ~ x + z + f, data = data) |>
    recipes::step_mutate(double_x = 2 * x, copied_f = f) |>
    recipes::step_interact(terms = ~ double_x:z + x:copied_f)
  rec <- suppressWarnings(recipes::prep(rec))
  expect_warning(compiled <- compile_workflow_recipe(rec),
                 "Unsupported.*step_mutate.*position 1")
  newdata <- data[1:4, c("y", "x", "z", "f")]
  expect_error(eval_workflow_recipe(compiled, newdata),
               "step_interact.*double_x.*copied_f.*position 2.*externally")
  newdata$double_x <- 2 * newdata$x
  newdata$copied_f <- newdata$f
  expected <- as.data.frame(recipes::bake(rec, newdata))
  expect_equal(eval_workflow_recipe(compiled, newdata), expected)
  expect_true(all(grepl("copied_f", compiled$ops[[1]]$names[[2]])))
  rec <- recipes::recipe(y ~ x + z, data = data) |>
    recipes::step_rename(feature = x) |>
    recipes::step_interact(terms = ~ feature:z) |>
    recipes::prep()
  expect_warning(compiled <- compile_workflow_recipe(rec),
                 "Unsupported.*step_rename.*position 1")
  newdata <- data[1:4, c("y", "x", "z")]
  newdata$feature <- newdata$x
  expected <- as.data.frame(recipes::bake(rec, newdata))
  actual <- eval_workflow_recipe(compiled, newdata)
  expect_equal(actual[, names(expected)], expected)
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
  rec <- recipes::recipe(y ~ f, data = recipe_compiler_data()) |>
    recipes::prep()
  compiled <- compile_workflow_recipe(rec)
  expect_error(eval_workflow_recipe(compiled, data.frame(f = "novel")),
               "Unknown factor levels.*f.*novel")
  expect_error(eval_workflow_recipe(compiled, data.frame(f = factor("novel"))),
               "Unknown factor levels.*f.*novel")
  expected <- data.frame(f = factor(c("a", NA), levels = c("a", "b", "c")))
  expect_equal(eval_workflow_recipe(compiled, data.frame(f = c("a", NA))), expected)
  expect_equal(nrow(eval_workflow_recipe(compiled, data.frame(f = c("a", NA)))), 2L)
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

test_that("a fitted workflow using burgle::step_abs preserves engine predictions", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  data <- recipe_compiler_data()
  data$x <- seq(-2, 2, length.out = nrow(data))
  data$z <- rep(c(-1, 2, -3, 4), 10)
  data$y <- 2 + 3 * abs(data$x) - abs(data$z) + sin(seq_len(nrow(data))) / 10
  rec <- recipes::recipe(y ~ x + z, data = data) |>
    burgle::step_abs(x, z) |>
    recipes::step_poly(x, degree = 2)
  model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  workflow <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(model) |>
    parsnip::fit(data = data)
  compact <- burgle(workflow)
  newdata <- data[c(3, 1, 20, 40), c("x", "z")]
  expected <- predict(workflow, new_data = newdata)$.pred
  observed <- predict(compact, newdata = newdata, original = TRUE, type = "lp")
  expect_equal(as.numeric(observed), expected, tolerance = 1e-10)
  expect_identical(vapply(compact$preprocessing$ops, `[[`, "", "type"), c("abs", "poly"))
})

test_that("every supported recipes step preserves fitted workflow predictions", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("splines2")
  skip_if_not_installed("dplyr")
  i <- seq_len(80)
  data <- data.frame(
    x = stats::plogis(2 * sin(i * sqrt(2)) + cos(i * sqrt(3))),
    z = stats::plogis(sin(i * sqrt(5)) + cos(i * sqrt(7)))
  )
  data$y <- 1 + 2 * data$x^2 - data$z + sin(i) / 10
  configs <- list(
    poly = list(degree = 3, options = list(raw = TRUE)),
    ns = list(options = list(knots = c(0.3, 0.6), Boundary.knots = c(0, 1))),
    bs = list(degree = 2, options = list(knots = c(0.3, 0.6),
                                       Boundary.knots = c(0, 1))),
    interact = list(terms = ~ x:z, sep = "__", keep_original_cols = TRUE),
    spline_b = list(deg_free = 5, degree = 2, complete_set = FALSE,
                    options = list(Boundary.knots = c(0, 1))),
    spline_natural = list(deg_free = 5, complete_set = FALSE,
                          options = list(Boundary.knots = c(0, 1))),
    harmonic = list(frequency = c(1, 2), cycle_size = 2, starting_val = 0.2),
    poly_bernstein = list(degree = 4, complete_set = FALSE, role = "predictor",
                          options = list(Boundary.knots = c(0, 1))),
    spline_monotone = list(deg_free = 5, degree = 2, complete_set = FALSE,
                           options = list(Boundary.knots = c(0, 1))),
    spline_convex = list(deg_free = 5, degree = 2, complete_set = FALSE,
                         options = list(Boundary.knots = c(0, 1))),
    spline_nonnegative = list(deg_free = 5, degree = 2, complete_set = FALSE,
                              options = list(Boundary.knots = c(0, 1))),
    log = list(base = 10, offset = 1),
    sqrt = list(),
    inverse = list(offset = 0.4),
    invlogit = list(),
    logit = list(offset = 0.01),
    ratio = list(denom = recipes::denom_vars(z), keep_original_cols = TRUE,
                 naming = function(top, bottom) paste0(top, "__over__", bottom)),
    lag = list(lag = c(1, 2), default = 0.2, prefix = "previous_",
               keep_original_cols = TRUE)
  )
  expect_length(configs, 18L)
  for (type in names(configs)) {
    rec <- recipes::recipe(y ~ x + z, data = data)
    columns <- if (type == "interact") list() else list(quote(x))
    rec <- do.call(getExportedValue("recipes", paste0("step_", type)),
                   c(list(rec), columns, configs[[type]]))
    model <- parsnip::linear_reg() |>
      parsnip::set_engine("lm")
    workflow <- workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(model) |>
      parsnip::fit(data = data)
    compact <- burgle(workflow)
    expect_identical(compact$preprocessing$ops[[1]]$type, type)
    expect_false(anyNA(compact$model$coef))
    newdata <- data[c(9, 1, 60, 4, 80), c("z", "x")]
    expected <- predict(workflow, new_data = newdata)$.pred
    observed <- predict(compact, newdata = newdata, original = TRUE, type = "lp")
    expect_equal(as.numeric(observed), expected, tolerance = 1e-8, info = type)
    one <- newdata[1, , drop = FALSE]
    expect_equal(as.numeric(predict(compact, newdata = one, original = TRUE, type = "lp")),
                 predict(workflow, new_data = one)$.pred,
                 tolerance = 1e-8, info = type)
  }
})

test_that("zero-step workflow prediction rejects novel factors before engine row loss", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  data <- recipe_compiler_data()
  model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  workflow <- workflows::workflow() |>
    workflows::add_recipe(recipes::recipe(y ~ f, data = data)) |>
    workflows::add_model(model) |>
    parsnip::fit(data = data)
  compact <- burgle(workflow)
  expect_error(predict(compact, newdata = data.frame(f = factor(rep("novel", 3)))),
               "Unknown factor levels.*f.*novel")
  expect_error(predict(compact, newdata = data.frame(f = rep("novel", 3))),
               "Unknown factor levels.*f.*novel")
})

test_that("raw nominal inputs normalize to trained levels before interactions and lags", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  data <- recipe_compiler_data()
  data$y <- 2 * data$x^2 + sin(seq_len(nrow(data))) / 10
  data$ordered <- ordered(rep(c("low", "mid", "high"), length.out = nrow(data)),
                          levels = c("low", "mid", "high"))
  spec <- recipes::recipe(y ~ x + f + ordered, data = data) |>
    recipes::step_interact(terms = ~ x:f + x:ordered)
  rec <- suppressWarnings(recipes::prep(spec))
  compiled <- compile_workflow_recipe(rec)
  model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  workflow <- suppressWarnings(
    workflows::workflow() |>
      workflows::add_recipe(spec) |>
      workflows::add_model(model) |>
      parsnip::fit(data = data)
  )
  compact <- burgle(workflow)
  expect_false(anyNA(compact$model$coef))
  variants <- list(
    c("c", "a", "c"),
    factor(c("c", "a", "c"), levels = c("c", "b", "a")),
    factor(rep("a", 3)),
    droplevels(factor(c("a", "c", "a"), levels = c("a", "b", "c")))
  )
  for (f in variants) {
    newdata <- data[1:3, c("y", "x", "f", "ordered")]
    newdata$f <- f
    newdata$ordered <- c("high", "low", "high")
    normalized <- newdata
    normalized$f <- factor(as.character(f), levels = levels(data$f))
    normalized$ordered <- ordered(newdata$ordered, levels = levels(data$ordered))
    ## Hardhat restores trained levels before recipe steps. Direct bake on
    ## old recipes versions needs the same normalized input for this oracle.
    expected <- as.data.frame(recipes::bake(rec, normalized))
    expect_equal(eval_workflow_recipe(compiled, newdata), expected)
    expected <- predict(workflow, new_data = newdata)$.pred
    expect_equal(as.numeric(predict(compact, newdata, original = TRUE, type = "lp")),
                 expected, tolerance = 1e-8)
  }
  lag <- recipe_compiler_step("lag", columns = "f", lag = 1)
  newdata <- data[1:3, ]
  newdata$f <- factor(rep("a", 3))
  actual <- eval_workflow_recipe(compile_workflow_recipe(lag), newdata)
  expect_identical(levels(actual$lag_1_f), levels(data$f))
  newdata$f <- "novel"
  expect_error(eval_workflow_recipe(compiled, newdata), "Unknown factor levels.*f")
  chars <- data.frame(x = c("a", "b", "a"), y = 1:3)
  rec <- recipes::prep(recipes::recipe(y ~ x, data = chars), strings_as_factors = FALSE)
  expect_recipe_compiler_bake(rec, chars)
})
