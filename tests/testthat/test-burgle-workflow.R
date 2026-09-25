workflow_baked_predictors <- function(wf, new_data) {
  recipes::bake(
    workflows::extract_recipe(wf),
    new_data = new_data
  )
}

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

  baked <- workflow_baked_predictors(wf, new_dat)
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

test_that("burgle.workflow compiles harmonic date features", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  dat <- data.frame(
    y = factor(rep(c("no", "yes"), length.out = 60)),
    d = as.Date("2024-01-01") + seq_len(60),
    x = seq(-1, 1, length.out = 60)
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ d + x, data = dat) |>
        recipes::step_harmonic(d, frequency = c(1, 2), cycle_size = 7)
    ) |>
    workflows::add_model(
      parsnip::logistic_reg() |>
        parsnip::set_engine("glm")
    ) |>
    workflows::fit(data = dat)

  bfit <- burgle(wf)
  new_dat <- data.frame(
    y = factor(rep("no", 5), levels = levels(dat$y)),
    d = as.Date("2024-04-01") + 0:4,
    x = seq(-0.5, 0.5, length.out = 5)
  )

  baked <- workflow_baked_predictors(wf, new_dat)
  expected <- stats::predict(
    workflows::extract_fit_engine(wf),
    newdata = baked,
    type = "link"
  )
  actual <- predict(bfit, newdata = new_dat, type = "lp")

  expect_equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-7)
})

test_that("burgle.workflow supports linear_reg workflows with compiled recipe terms", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  set.seed(202)
  dat <- data.frame(
    y = stats::rnorm(80),
    x1 = stats::rnorm(80),
    x2 = stats::runif(80, 0.2, 2),
    grp = factor(sample(c("a", "b", "c"), 80, replace = TRUE))
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ x1 + x2 + grp, data = dat) |>
        recipes::step_ns(x2, deg_free = 4)
    ) |>
    workflows::add_model(
      parsnip::linear_reg() |>
        parsnip::set_engine("lm")
    ) |>
    workflows::fit(data = dat)

  bfit <- burgle(wf)
  new_dat <- data.frame(
    y = 0,
    x1 = seq(-1, 1, length.out = 6),
    x2 = seq(0.3, 1.8, length.out = 6),
    grp = factor(c("a", "b", "c", "a", "b", "c"), levels = levels(dat$grp))
  )

  baked <- workflow_baked_predictors(wf, new_dat)
  expected <- stats::predict(workflows::extract_fit_engine(wf), newdata = baked)
  actual <- predict(bfit, newdata = new_dat, type = "lp")

  expect_s3_class(bfit, "burgle_lm")
  expect_equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-7)
})

test_that("burgle.workflow supports multinom workflows with compiled recipe terms", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  set.seed(303)
  n <- 120
  dat <- data.frame(
    y = factor(sample(c("low", "mid", "high"), n, replace = TRUE)),
    x1 = stats::rnorm(n),
    x2 = stats::runif(n, 0.2, 2),
    grp = factor(sample(c("a", "b"), n, replace = TRUE))
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ x1 + x2 + grp, data = dat) |>
        recipes::step_bs(x2, deg_free = 4)
    ) |>
    workflows::add_model(
      parsnip::multinom_reg() |>
        parsnip::set_engine("nnet", trace = FALSE)
    ) |>
    workflows::fit(data = dat)

  bfit <- burgle(wf)
  new_dat <- data.frame(
    y = factor("low", levels = levels(dat$y)),
    x1 = seq(-1, 1, length.out = 5),
    x2 = seq(0.25, 1.75, length.out = 5),
    grp = factor(c("a", "b", "a", "b", "a"), levels = levels(dat$grp))
  )

  baked <- workflow_baked_predictors(wf, new_dat)
  expected <- stats::predict(
    workflows::extract_fit_engine(wf),
    newdata = baked,
    type = "probs"
  )
  actual <- predict(bfit, newdata = new_dat, type = "odds")
  compiled_mm <- stats::model.matrix(
    bfit$terms,
    data = new_dat,
    xlev = bfit$xlevels,
    contrasts.arg = bfit$contrasts
  )
  expected_coef_names <- unlist(lapply(
    bfit$rlev[-1],
    function(x) paste0(x, ":", colnames(compiled_mm))
  ))

  expect_s3_class(bfit, "burgle_multinom")
  expect_equal(names(bfit$coef), expected_coef_names)
  expect_equal(unname(actual), unname(expected), tolerance = 1e-6)
})

test_that("burgle.workflow supports proportional hazards workflows when censored is available", {
  skip_if_not_installed("censored")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("survival")
  skip_if_not_installed("workflows")

  set.seed(404)
  n <- 100
  dat <- data.frame(
    time = stats::rexp(n, rate = 0.1),
    status = stats::rbinom(n, 1, 0.7),
    x1 = stats::rnorm(n),
    x2 = stats::runif(n, 0.2, 2),
    grp = factor(sample(c("a", "b"), n, replace = TRUE))
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(survival::Surv(time, status) ~ x1 + x2 + grp, data = dat) |>
        recipes::step_ns(x2, deg_free = 3)
    ) |>
    workflows::add_model(
      censored::proportional_hazards() |>
        parsnip::set_engine("survival")
    ) |>
    workflows::fit(data = dat)

  bfit <- burgle(wf)
  new_dat <- data.frame(
    time = 1,
    status = 1,
    x1 = seq(-1, 1, length.out = 5),
    x2 = seq(0.3, 1.7, length.out = 5),
    grp = factor(c("a", "b", "a", "b", "a"), levels = levels(dat$grp))
  )

  baked <- workflow_baked_predictors(wf, new_dat)
  expected <- stats::predict(
    workflows::extract_fit_engine(wf),
    newdata = baked,
    type = "lp"
  )
  actual <- predict(bfit, newdata = new_dat, type = "lp")
  compiled_mm <- stats::model.matrix(
    bfit$terms,
    data = new_dat,
    xlev = bfit$xlevels,
    contrasts.arg = bfit$contrasts
  )[, -1, drop = FALSE]

  expect_s3_class(bfit, "burgle_coxph")
  expect_equal(names(bfit$coef), colnames(compiled_mm))
  expect_equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-7)
})

test_that("burgle.workflow supports rms cph workflows when censored is available", {
  skip_if_not_installed("censored")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  skip_if_not_installed("workflows")

  set.seed(405)
  n <- 80
  dat <- data.frame(
    time = stats::rexp(n, rate = 0.1),
    status = stats::rbinom(n, 1, 0.7),
    x1 = stats::rnorm(n),
    x2 = stats::runif(n, 0.2, 2)
  )

  dd <- rms::datadist(dat)
  old_opt <- options(datadist = "dd")
  on.exit(options(old_opt), add = TRUE)

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(survival::Surv(time, status) ~ x1 + x2, data = dat) |>
        recipes::step_bs(x2, deg_free = 4)
    ) |>
    workflows::add_model(
      censored::proportional_hazards() |>
        parsnip::set_engine("rms", x = TRUE, y = TRUE, surv = TRUE)
    ) |>
    workflows::fit(data = dat)

  bfit <- burgle(wf)
  new_dat <- data.frame(
    time = 1,
    status = 1,
    x1 = seq(-1, 1, length.out = 4),
    x2 = seq(0.3, 1.7, length.out = 4)
  )

  compiled_mm <- stats::model.matrix(
    bfit$terms,
    data = new_dat,
    xlev = bfit$xlevels,
    contrasts.arg = bfit$contrasts
  )[, -1, drop = FALSE]

  expect_s3_class(bfit, "burgle_cph")
  expect_equal(names(bfit$coef), colnames(compiled_mm))
})

test_that("burgle.workflow supports flexsurv workflows when censored is available", {
  skip_if_not_installed("censored")
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("survival")
  skip_if_not_installed("workflows")

  set.seed(406)
  n <- 80
  dat <- data.frame(
    time = stats::rexp(n, rate = 0.1),
    status = stats::rbinom(n, 1, 0.7),
    x1 = stats::rnorm(n),
    x2 = stats::runif(n, 0.2, 2)
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(survival::Surv(time, status) ~ x1 + x2, data = dat) |>
        recipes::step_ns(x2, deg_free = 3)
    ) |>
    workflows::add_model(
      censored::survival_reg() |>
        parsnip::set_engine("flexsurv", dist = "weibull")
    ) |>
    workflows::fit(data = dat)

  bfit <- burgle(wf)
  new_dat <- data.frame(
    time = 1,
    status = 1,
    x1 = seq(-1, 1, length.out = 4),
    x2 = seq(0.3, 1.7, length.out = 4)
  )

  compiled_mm <- stats::model.matrix(
    bfit$terms,
    data = new_dat,
    xlev = bfit$xlevels,
    contrasts.arg = bfit$contrasts
  )[, -1, drop = FALSE]
  parameter_indices <- bfit$pars_indices
  if (is.null(parameter_indices)) {
    parameter_indices <- bfit$pars_indeces
  }
  covariate_indices <- setdiff(seq_along(bfit$coef), parameter_indices)

  expect_s3_class(bfit, "burgle_flexsurvreg")
  expect_equal(names(bfit$coef)[covariate_indices], colnames(compiled_mm))
  expect_equal(rownames(bfit$cov), names(bfit$coef))
  expect_equal(colnames(bfit$cov), names(bfit$coef))
})

test_that("burgle.workflow rejects transformed recipes for non-terms engines", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("randomForestSRC")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")

  set.seed(505)
  dat <- data.frame(
    y = stats::rnorm(60),
    x1 = stats::rnorm(60),
    x2 = stats::runif(60, 0.2, 2)
  )

  wf <- workflows::workflow() |>
    workflows::add_recipe(
      recipes::recipe(y ~ x1 + x2, data = dat) |>
        recipes::step_ns(x2, deg_free = 3)
    ) |>
    workflows::add_model(
      parsnip::rand_forest(mode = "regression", trees = 20) |>
        parsnip::set_engine("randomForestSRC")
    ) |>
    workflows::fit(data = dat)

  expect_error(
    burgle(wf),
    "does not support recipe preprocessing for burgled objects of class `burgle_rfsrc`"
  )
})
