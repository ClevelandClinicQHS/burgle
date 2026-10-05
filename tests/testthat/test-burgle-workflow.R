test_that("burgle.workflow rejects formula preprocessors", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")

  dat <- iris
  dat$y <- factor(ifelse(dat$Species == "setosa", "yes", "no"))

  wf <- suppressWarnings(
    workflows::workflow() |>
      workflows::add_formula(y ~ Sepal.Length + Species) |>
      workflows::add_model(
        parsnip::logistic_reg() |>
          parsnip::set_engine("glm")
      ) |>
      workflows::fit(data = dat)
  )

  expect_error(
    burgle(wf),
    "Only workflows with recipe preprocessors are supported"
  )
})

test_that("burgle.workflow compiles trained bs and ns steps for glm", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  set.seed(101)
  dat <- data.frame(
    y = factor(ifelse(stats::runif(80) > 0.5, "yes", "no")),
    x1 = stats::runif(80, 0.2, 2),
    x2 = stats::runif(80, 0.2, 2),
    z = stats::rnorm(80)
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ x1 + x2 + z, data = dat) |>
        recipes::step_ns(x1, deg_free = 3) |>
        recipes::step_bs(x2, deg_free = 4, degree = 2)
    ) |>
    workflows::add_model(
      parsnip::logistic_reg() |>
        parsnip::set_engine("glm")
    ) |>
    workflows::fit(data = dat)

  bfit <- burgle(wf)
  expect_s3_class(bfit, "burgle_glm")
  expect_s3_class(bfit$terms, "terms")

  new_dat <- data.frame(
    y = factor(rep("no", 6), levels = levels(dat$y)),
    x1 = seq(0.25, 1.75, length.out = 6),
    x2 = seq(0.3, 1.8, length.out = 6),
    z = seq(-1, 1, length.out = 6)
  )

  baked <- recipes::bake(
    workflows::extract_recipe(wf, estimated = TRUE),
    new_data = new_dat
  )

  expected <- stats::predict(
    workflows::extract_fit_engine(wf),
    newdata = baked,
    type = "link"
  )

  actual <- predict(bfit, newdata = new_dat, type = "lp")

  expect_equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-7)
})

test_that("burgle.workflow compiles trained bs and ns steps for lm", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  set.seed(202)
  dat <- data.frame(
    y = stats::rnorm(80),
    x1 = stats::runif(80, 0.2, 2),
    x2 = stats::runif(80, 0.2, 2),
    z = stats::rnorm(80)
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ x1 + x2 + z, data = dat) |>
        recipes::step_ns(x1, deg_free = 3) |>
        recipes::step_bs(x2, deg_free = 4, degree = 2)
    ) |>
    workflows::add_model(
      parsnip::linear_reg() |>
        parsnip::set_engine("lm")
    ) |>
    workflows::fit(data = dat)

  bfit <- burgle(wf)
  expect_s3_class(bfit, "burgle_lm")
  expect_s3_class(bfit$terms, "terms")

  new_dat <- data.frame(
    y = 0,
    x1 = seq(0.25, 1.75, length.out = 6),
    x2 = seq(0.3, 1.8, length.out = 6),
    z = seq(-1, 1, length.out = 6)
  )

  baked <- recipes::bake(
    workflows::extract_recipe(wf, estimated = TRUE),
    new_data = new_dat
  )

  expected <- stats::predict(
    workflows::extract_fit_engine(wf),
    newdata = baked
  )

  actual <- predict(bfit, newdata = new_dat, type = "lp")

  expect_equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-7)
})

test_that("burgle.workflow stores numeric spline knots and boundaries", {
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(~ x1 + x2, data = data.frame(x1 = 1:10, x2 = 1:10)) |>
    recipes::step_ns(x1, deg_free = 3) |>
    recipes::step_bs(x2, deg_free = 4, degree = 2) |>
    recipes::prep()

  specs <- workflow_spline_specs(rec)

  expect_true(all(vapply(specs, function(x) is.numeric(x$knots), logical(1))))
  expect_true(all(vapply(specs, function(x) is.numeric(x$boundary), logical(1))))
})

test_that("burgle.workflow warns and skips unsupported recipe steps", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  dat <- data.frame(
    y = factor(rep(c("no", "yes"), length.out = 40)),
    x1 = stats::runif(40),
    x2 = stats::runif(40)
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ x1 + x2, data = dat) |>
        recipes::step_mutate(x3 = x1 + x2)
    ) |>
    workflows::add_model(
      parsnip::logistic_reg() |>
        parsnip::set_engine("glm")
    ) |>
    workflows::fit(data = dat)

  expect_warning(
    bfit <- burgle(wf),
    "Unsupported recipe step\\(s\\) were skipped"
  )
  expect_s3_class(bfit, "burgle_glm")
  expect_s3_class(bfit$terms, "terms")
})

test_that("burgle.workflow handles already response-free burgle terms", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  set.seed(303)
  dat <- data.frame(
    y = stats::rnorm(60),
    x1 = stats::runif(60, 0.2, 2),
    x2 = stats::runif(60, 0.2, 2),
    z = stats::rnorm(60)
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ x1 + x2 + z, data = dat) |>
        recipes::step_mutate(z2 = z * 2) |>
        recipes::step_ns(x1, deg_free = 3) |>
        recipes::step_bs(x2, deg_free = 4, degree = 2)
    ) |>
    workflows::add_model(
      parsnip::linear_reg() |>
        parsnip::set_engine("lm")
    ) |>
    workflows::fit(data = dat)

  expect_warning(
    bfit <- burgle(wf),
    "Unsupported recipe step\\(s\\) were skipped"
  )

  expect_s3_class(bfit, "burgle_lm")
  expect_s3_class(bfit$terms, "terms")
})
