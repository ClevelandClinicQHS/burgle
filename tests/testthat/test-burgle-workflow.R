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
    "does not support formula-preprocessor workflows"
  )
})

test_that("burgle.workflow compiles supported recipe steps on raw newdata", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("splines2")
  skip_if_not_installed("workflows")

  set.seed(101)
  n <- 120
  dat <- data.frame(
    y = factor(ifelse(stats::runif(n) > 0.5, "yes", "no")),
    x1 = stats::rnorm(n),
    x2 = stats::runif(n, 0.2, 2),
    x3 = stats::runif(n, 0.2, 2),
    x4 = stats::runif(n, 0.2, 2),
    x5 = stats::runif(n, 0.2, 2),
    x6 = stats::runif(n, 0.2, 2),
    x7 = stats::runif(n, 0.2, 2),
    x8 = stats::runif(n, 0.2, 2),
    x9 = stats::runif(n, 0.2, 2),
    x10 = stats::runif(n, 1, 5),
    x11 = stats::runif(n, 1, 5),
    x12 = stats::runif(n, 1, 5),
    x13 = stats::rnorm(n),
    p1 = stats::runif(n, 0.1, 0.9),
    t1 = seq_len(n)
  )

  rec <- recipes::recipe(y ~ ., data = dat) |>
    recipes::step_poly(x1, degree = 2) |>
    recipes::step_ns(x2, deg_free = 3) |>
    recipes::step_bs(x3, deg_free = 4, degree = 2) |>
    recipes::step_spline_b(x4, deg_free = 4) |>
    recipes::step_spline_natural(x5, deg_free = 4) |>
    recipes::step_harmonic(t1, frequency = c(1, 2), cycle_size = n) |>
    recipes::step_poly_bernstein(x6, degree = 3) |>
    recipes::step_spline_monotone(x7, deg_free = 4) |>
    recipes::step_spline_convex(x8, deg_free = 4) |>
    recipes::step_spline_nonnegative(x9, deg_free = 4) |>
    recipes::step_log(x10, offset = 1) |>
    recipes::step_sqrt(x11) |>
    recipes::step_inverse(x12, offset = 1) |>
    recipes::step_invlogit(x13) |>
    recipes::step_logit(p1, offset = 1e-4) |>
    recipes::step_ratio(x10, denom = x11, keep_original_cols = TRUE) |>
    recipes::step_interact(terms = ~ x10_o_x11:x12)

  wf <- suppressWarnings(
    workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(
        parsnip::logistic_reg() |>
          parsnip::set_engine("glm")
      ) |>
      workflows::fit(data = dat)
  )

  bfit <- burgle(wf)
  new_dat <- data.frame(
    y = factor(rep("no", 10), levels = levels(dat$y)),
    x1 = seq(-1.5, 1.5, length.out = 10),
    x2 = seq(0.25, 1.95, length.out = 10),
    x3 = seq(0.3, 1.9, length.out = 10),
    x4 = seq(0.35, 1.85, length.out = 10),
    x5 = seq(0.4, 1.8, length.out = 10),
    x6 = seq(0.45, 1.75, length.out = 10),
    x7 = seq(0.5, 1.7, length.out = 10),
    x8 = seq(0.55, 1.65, length.out = 10),
    x9 = seq(0.6, 1.6, length.out = 10),
    x10 = seq(1.2, 4.8, length.out = 10),
    x11 = seq(1.1, 4.6, length.out = 10),
    x12 = seq(1.3, 4.9, length.out = 10),
    x13 = seq(-2, 2, length.out = 10),
    p1 = seq(0.15, 0.85, length.out = 10),
    t1 = seq(5, 95, length.out = 10)
  )

  baked <- recipes::bake(
    workflows::extract_recipe(wf),
    new_data = new_dat
  )
  expected <- stats::predict(
    workflows::extract_fit_engine(wf),
    newdata = baked,
    type = "link"
  )
  actual <- predict(bfit, newdata = new_dat, type = "lp")

  expect_true("splines2" %in% bfit$workflow_required_pkgs)
  expect_equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-7)
})

test_that("burgle.workflow rejects unsafe step_lag recipes", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  dat <- data.frame(
    y = factor(rep(c("no", "yes"), length.out = 30)),
    x = stats::runif(30),
    idx = seq_len(30)
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ x + idx, data = dat) |>
        recipes::step_lag(x, lag = 1)
    ) |>
    workflows::add_model(
      parsnip::logistic_reg() |>
        parsnip::set_engine("glm")
    ) |>
    workflows::fit(data = dat)

  expect_error(
    burgle(wf),
    "step_lag\\(\\) depends on row order and cross-row state"
  )
})

test_that("burgle.workflow rejects unsafe factor interactions", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  dat <- data.frame(
    y = factor(rep(c("no", "yes"), length.out = 40)),
    grp = factor(rep(c("a", "b"), length.out = 40)),
    x = stats::runif(40)
  )

  wf <- suppressWarnings(
    workflows::workflow() |>
      workflows::add_recipe(
        recipes::recipe(y ~ grp + x, data = dat) |>
          recipes::step_interact(terms = ~ grp:x)
      ) |>
      workflows::add_model(
        parsnip::logistic_reg() |>
          parsnip::set_engine("glm")
      ) |>
      workflows::fit(data = dat)
  )

  expect_error(
    burgle(wf),
    "step_interact\\(\\) is only supported when every referenced column is scalar"
  )
})
