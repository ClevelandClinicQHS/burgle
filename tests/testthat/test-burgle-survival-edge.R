test_that("burgle_coxph handles weighted subset models with interactions and offsets", {
  lung_edge <- subset(survival::lung, !is.na(ph.ecog))
  lung_edge$status <- lung_edge$status - 1
  lung_edge$sex_f <- factor(lung_edge$sex, levels = c(1, 2, 3), labels = c("male", "female", "unused"))
  lung_edge$grp <- factor(rep(c("A", "B"), length.out = nrow(lung_edge)), levels = c("A", "B", "C"))
  lung_edge$off <- log(lung_edge$ph.ecog + 1)
  lung_edge$w <- seq_len(nrow(lung_edge)) %% 5 + 1

  fit <- survival::coxph(
    survival::Surv(time, status) ~ age * sex_f + grp + stats::offset(off),
    data = lung_edge,
    weights = w,
    subset = 1:150
  )
  bfit <- burgle(fit)
  risk <- predict(bfit, newdata = head(lung_edge), type = "risk", times = c(100, 500))

  expect_equal(dim(risk), c(6, 2))
  expect_true(all(risk >= 0 & risk <= 1))
})

test_that("burgle_coxph predicts risk for true strata-only models", {
  strata <- survival::strata
  lung_edge <- subset(survival::lung, !is.na(ph.ecog))
  lung_edge$status <- lung_edge$status - 1
  lung_edge$sex_f <- factor(lung_edge$sex, levels = c(1, 2, 3), labels = c("male", "female", "unused"))

  fit <- survival::coxph(survival::Surv(time, status) ~ strata(sex_f), data = lung_edge)
  bfit <- burgle(fit)
  risk <- predict(bfit, newdata = head(lung_edge), type = "risk", times = c(100, 500))

  expect_equal(dim(risk), c(6, 2))
  expect_true(all(risk >= 0 & risk <= 1))
})

test_that("predict_time on strata-only cox models still breaks", {
  strata <- survival::strata
  lung_edge <- subset(survival::lung, !is.na(ph.ecog))
  lung_edge$status <- lung_edge$status - 1
  lung_edge$sex_f <- factor(lung_edge$sex, levels = c(1, 2, 3), labels = c("male", "female", "unused"))

  fit <- survival::coxph(survival::Surv(time, status) ~ strata(sex_f), data = lung_edge)
  bfit <- burgle(fit)

  expect_error(predict_time(bfit, newdata = head(lung_edge)))
})

test_that("burgle_coxph errors on truly new factor levels", {
  lung_edge <- subset(survival::lung, !is.na(ph.ecog))
  lung_edge$status <- lung_edge$status - 1
  lung_edge$sex_f <- factor(lung_edge$sex)

  fit <- survival::coxph(survival::Surv(time, status) ~ sex_f + age, data = lung_edge)
  bfit <- burgle(fit)

  new_data <- head(lung_edge, 2)
  new_data$sex_f <- factor(c("3", "3"))

  expect_error(predict(bfit, newdata = new_data, type = "risk", times = 100), "new level")
})

test_that("burgle_CauseSpecificCox handles mixed predictors and interactions", {
  set.seed(42)
  lung_cr <- data.frame(
    time = rexp(120, rate = 0.01),
    cause = sample(c(0L, 1L, 2L), 120, replace = TRUE, prob = c(0.4, 0.3, 0.3)),
    age = rnorm(120, 60, 8),
    sex = sample(c(1, 2), 120, replace = TRUE),
    grp = factor(sample(c("A", "B"), 120, replace = TRUE))
  )

  fit <- riskRegression::CSC(prodlim::Hist(time, cause) ~ age * grp + factor(sex), data = lung_cr)
  bfit <- burgle(fit)
  risk <- predict(bfit, newdata = head(lung_cr), type = "risk", cause = 1, times = c(100, 500))
  lp <- predict(bfit, newdata = head(lung_cr), type = "lp", cause = 1)

  expect_equal(dim(risk), c(6, 2))
  expect_true(all(risk >= 0 & risk <= 1))
  expect_equal(nrow(lp), 6)
})

test_that("burgle_CauseSpecificCox validates cause and times", {
  set.seed(99)
  lung_cr <- data.frame(
    time = rexp(90, rate = 0.01),
    cause = sample(c(0L, 1L, 2L), 90, replace = TRUE, prob = c(0.4, 0.3, 0.3)),
    age = rnorm(90, 60, 8),
    sex = sample(c(1, 2), 90, replace = TRUE)
  )

  fit <- riskRegression::CSC(prodlim::Hist(time, cause) ~ age + factor(sex), data = lung_cr)
  bfit <- burgle(fit)

  expect_error(predict(bfit, newdata = head(lung_cr), type = "risk", cause = 1), "times is missing")
  expect_error(predict(bfit, newdata = head(lung_cr), type = "risk", cause = 3, times = 100), "Invalid cause")
})
