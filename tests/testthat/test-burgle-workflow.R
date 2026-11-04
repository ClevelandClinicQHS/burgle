workflow_ns_fit <- function(recipe, data, model = parsnip::linear_reg()) {
  workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(model) |>
    parsnip::fit(data = data)
}

skip_workflow_dependencies <- function() {
  skip_if_not_installed("workflows")
  skip_if_not_installed("recipes")
  skip_if_not_installed("parsnip")
}

test_that("workflow terms recover trained knots and predict on raw data", {
  skip_workflow_dependencies()
  data <- mtcars
  data$cyl <- factor(data$cyl)
  recipe <- recipes::recipe(mpg ~ disp + wt + cyl, data = data) |>
    recipes::step_ns(disp, deg_free = 4)
  fit <- workflow_ns_fit(recipe, data)
  result <- burgle(fit)
  basis <- workflows::extract_recipe(fit)$steps[[1]]$objects$disp
  variables <- as.list(attr(result$terms, "variables"))[-1]
  ns_call <- variables[[which(vapply(variables, function(x) {
    is.call(x) && identical(x[[1]], quote(splines::ns))
  }, logical(1)))]]

  expect_identical(ns_call[[2]], quote(disp))
  expect_equal(ns_call$knots, unname(attr(basis, "knots")))
  expect_equal(ns_call$Boundary.knots, unname(attr(basis, "Boundary.knots")))
  expect_identical(ns_call$intercept, attr(basis, "intercept"))
  expect_null(attr(result$terms, ".Environment"))
  expect_false("recipe" %in% names(result))
  expect_false(any(grepl("disp_ns_", attr(result$terms, "term.labels"))))

  newdata <- data.frame(disp = c(30, 200, 600), wt = c(2, 3, 5),
                        cyl = factor(c(4, 6, 8), levels = levels(data$cyl)))
  expect_equal(as.numeric(predict(result, newdata)),
               as.numeric(predict(fit, new_data = newdata)$.pred),
               tolerance = 1e-8)
  refit <- lm(stats::update.formula(stats::formula(result$terms), mpg ~ .),
              data = data)
  expect_equal(as.numeric(predict(refit, newdata)),
               as.numeric(predict(result, newdata)), tolerance = 1e-8)
  expect_equal(unname(result$cov), unname(stats::vcov(refit)), tolerance = 1e-8)
})

test_that("multiple splines and nonsyntactic names retain their order", {
  skip_workflow_dependencies()
  data <- mtcars
  data[["log hgt ratio"]] <- log(data$disp)
  recipe <- recipes::recipe(mpg ~ `log hgt ratio` + wt + cyl, data = data) |>
    recipes::step_ns(`log hgt ratio`, wt, deg_free = 3)
  fit <- workflow_ns_fit(recipe, data)
  result <- burgle(fit)
  expect_equal(as.numeric(predict(result, head(data))),
               predict(fit, new_data = head(data))$.pred, tolerance = 1e-8)
})

test_that("nonconsecutive spline columns reorder coefficients and covariance", {
  skip_workflow_dependencies()
  recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
    recipes::step_ns(disp, deg_free = 3)
  fit <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(parsnip::linear_reg(),
                         formula = mpg ~ disp_ns_3 + wt + disp_ns_1 + disp_ns_2) |>
    parsnip::fit(data = mtcars)
  result <- burgle(fit)
  refit <- lm(stats::update.formula(stats::formula(result$terms), mpg ~ .),
              data = mtcars)
  expect_equal(unname(result$coef), unname(coef(refit)), tolerance = 1e-8)
  expect_equal(unname(result$cov), unname(stats::vcov(refit)), tolerance = 1e-8)
  expect_equal(as.numeric(predict(result, head(mtcars))),
               predict(fit, new_data = head(mtcars))$.pred, tolerance = 1e-8)
})

test_that("kept original columns preserve aliased coefficients", {
  skip_workflow_dependencies()
  recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
    recipes::step_ns(disp, deg_free = 3, keep_original_cols = TRUE)
  fit <- workflow_ns_fit(recipe, mtcars)
  result <- burgle(fit)
  engine <- workflows::extract_fit_engine(fit)
  coefficients <- coef(engine)
  coefficients[is.na(coefficients)] <- 0
  matrix <- stats::model.matrix(engine)
  expect_warning(predictions <- predict(result, mtcars), "Coefficient\\(s\\) NA")
  expect_equal(as.numeric(predictions), as.numeric(matrix %*% coefficients),
               tolerance = 1e-8)
})

test_that("explicit knots, intercept and zero-intercept fits are preserved", {
  skip_workflow_dependencies()
  recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
    recipes::step_ns(disp, deg_free = 4, options = list(
      knots = c(150, 300), Boundary.knots = c(50, 500), intercept = TRUE
    ))
  fit <- workflows::workflow() |>
    workflows::add_recipe(recipe) |>
    workflows::add_model(parsnip::linear_reg(), formula = mpg ~ 0 + .) |>
    parsnip::fit(data = mtcars)
  result <- burgle(fit)
  expect_identical(attr(result$terms, "intercept"), 0L)
  expect_equal(as.numeric(predict(result, head(mtcars))),
               predict(fit, new_data = head(mtcars))$.pred, tolerance = 1e-8)
})

test_that("one-column and double-digit spline expansions preserve predictions", {
  skip_workflow_dependencies()
  for (df in c(1, 10)) {
    recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
      recipes::step_ns(disp, deg_free = df)
    fit <- workflow_ns_fit(recipe, mtcars)
    expect_equal(as.numeric(predict(burgle(fit), head(mtcars))),
                 predict(fit, new_data = head(mtcars))$.pred, tolerance = 1e-8)
  }
})

test_that("glm workflows retain link, response and coefficient simulation behavior", {
  skip_workflow_dependencies()
  data <- mtcars
  data$am <- factor(data$am)
  recipe <- recipes::recipe(am ~ disp + wt, data = data) |>
    recipes::step_ns(disp, deg_free = 2)
  fit <- workflow_ns_fit(recipe, data, parsnip::logistic_reg())
  result <- burgle(fit)
  expect_s3_class(result, "burgle_glm")
  engine <- workflows::extract_fit_engine(fit)
  baked <- recipes::bake(workflows::extract_recipe(fit), head(data))
  expect_equal(as.numeric(predict(result, head(mtcars))),
               as.numeric(predict(engine, baked, type = "link")), tolerance = 1e-8)
  expect_equal(as.numeric(unlist(predict(result, head(mtcars), type = "link"))),
               as.numeric(predict(engine, baked, type = "response")), tolerance = 1e-8)
  expect_equal(dim(predict(result, head(mtcars), original = FALSE, draws = 3)),
               c(6L, 3L))
})

test_that("legacy burgle objects use engine terms and update character formulas", {
  skip_workflow_dependencies()
  skip_if_not_installed("testthat", "3.2.0")
  recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
    recipes::step_ns(disp, deg_free = 3)
  fit <- workflow_ns_fit(recipe, mtcars)
  engine <- workflows::extract_fit_engine(fit)
  legacy <- burgle(engine)
  legacy$formula <- attr(stats::terms(engine), "term.labels")
  legacy$rss <- legacy$mse
  legacy[c("terms", "contrasts", "mse")] <- NULL
  testthat::local_mocked_bindings(burgle = function(object, ...) legacy,
                                  .package = "burgle")
  result <- burgle:::burgle.workflow(fit)

  expect_s3_class(result$terms, "terms")
  expect_true(any(grepl("splines::ns", result$formula)))
  expect_false(any(grepl("disp_ns_", result$formula)))
  matrix <- stats::model.matrix(stats::reformulate(result$formula),
                                data = mtcars, xlev = result$xlevels)
  expect_equal(as.numeric(matrix %*% result$coef),
               predict(fit, new_data = mtcars)$.pred, tolerance = 1e-8)
})

test_that("workflows without recipe steps delegate to the engine method", {
  skip_workflow_dependencies()
  fit <- workflows::workflow() |>
    workflows::add_formula(mpg ~ disp + wt) |>
    workflows::add_model(parsnip::linear_reg()) |>
    parsnip::fit(data = mtcars)
  expect_equal(burgle(fit), burgle(workflows::extract_fit_engine(fit)))
  fit <- workflow_ns_fit(recipes::recipe(mpg ~ disp + wt, mtcars), mtcars)
  expect_equal(burgle(fit), burgle(workflows::extract_fit_engine(fit)))
})

test_that("unsupported preprocessing warns but spline reconstruction continues", {
  skip_workflow_dependencies()
  recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
    recipes::step_normalize(wt) |>
    recipes::step_ns(disp)
  fit <- workflow_ns_fit(recipe, mtcars)
  expect_warning(result <- burgle(fit), "Unsupported recipe steps.*step_normalize")
  expect_true(any(grepl("splines::ns", attr(result$terms, "term.labels"))))
  newdata <- head(mtcars)
  baked <- recipes::bake(workflows::extract_recipe(fit), newdata)
  newdata$wt <- baked$wt
  expect_equal(as.numeric(predict(result, newdata)),
               predict(fit, new_data = head(mtcars))$.pred, tolerance = 1e-8)
  recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
    recipes::step_normalize(wt)
  fit <- workflow_ns_fit(recipe, mtcars)
  expect_warning(result <- burgle(fit), "Unsupported recipe steps")
  expect_equal(result, burgle(workflows::extract_fit_engine(fit)))
})

test_that("skipped spline steps and untrained workflows fail explicitly", {
  skip_workflow_dependencies()
  recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
    recipes::step_ns(disp, skip = TRUE)
  expect_error(burgle(workflow_ns_fit(recipe, mtcars)), "Skipped step_ns")
  expect_error(burgle(workflows::workflow()), "untrained workflow")
})

test_that("partial and interacting spline expansions fail explicitly", {
  skip_workflow_dependencies()
  recipe <- recipes::recipe(mpg ~ disp + wt, data = mtcars) |>
    recipes::step_ns(disp, deg_free = 2)
  for (formula in list(mpg ~ disp_ns_1 + wt,
                       mpg ~ disp_ns_1 * wt + disp_ns_2)) {
    fit <- workflows::workflow() |>
      workflows::add_recipe(recipe) |>
      workflows::add_model(parsnip::linear_reg(), formula = formula) |>
      parsnip::fit(data = mtcars)
    expect_error(burgle(fit), "complete, additive spline expansions")
  }
})
