## ##############################################################################
## CauseSpecificCox (CSC): Object Structure Tests
## ##############################################################################

test_that("burgle.CauseSpecificCox returns correct class", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_s3_class(bfit, "burgle_CauseSpecificCox")
})

test_that("burgle.CauseSpecificCox preserves required components", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_true(!is.null(bfit$cumhazards))
  expect_true(!is.null(bfit$hazards))
  expect_true(!is.null(bfit$eventTimes))
  expect_true(!is.null(bfit$coef))
  expect_true(!is.null(bfit$cov))
  expect_true(!is.null(bfit$terms))
})

test_that("burgle.CauseSpecificCox preserves coefficients for each cause", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_equal(bfit$coef[[1]], stats::coef(fit$models[[1]]))
  expect_equal(bfit$coef[[2]], stats::coef(fit$models[[2]]))
})

test_that("burgle.CauseSpecificCox number of model components matches number of causes", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  nCauses <- length(fit$models)
  expect_equal(length(bfit$cumhazards), nCauses)
  expect_equal(length(bfit$hazards), nCauses)
  expect_equal(length(bfit$coef), nCauses)
})

## ##############################################################################
## CauseSpecificCox: Prediction Tests - Linear Predictor (type = "lp")
## ##############################################################################

test_that("predict.burgle_CauseSpecificCox type='lp' returns correct number of rows", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  newdata <- head(Melanoma, 5)
  result <- predict(bfit, newdata = newdata, type = "lp")

  expect_true(nrow(result) == nrow(newdata))
})

test_that("predict.burgle_CauseSpecificCox type='lp' returns numeric values", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(Melanoma), type = "lp")

  expect_true(is.numeric(result))
  expect_false(any(is.na(result)))
})

## ##############################################################################
## CauseSpecificCox: Prediction Tests - Risk Type (type = "risk")
## ##############################################################################

test_that("predict.burgle_CauseSpecificCox risk type cause=1 returns valid probabilities", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(Melanoma), original = TRUE, type = "risk",
                    cause = 1, times = 1000)

  expect_true(all(result >= 0 & result <= 1))
  expect_equal(nrow(result), nrow(head(Melanoma)))
})

test_that("predict.burgle_CauseSpecificCox risk type cause=2 returns valid probabilities", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(Melanoma), original = TRUE, type = "risk",
                    cause = 2, times = 1000)

  expect_true(all(result >= 0 & result <= 1))
  expect_equal(nrow(result), nrow(head(Melanoma)))
})

test_that("predict.burgle_CauseSpecificCox risk matches riskRegression::predictRisk for cause 1", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  newdata <- head(Melanoma)
  time_point <- 1000

  risk_burgle <- predict(bfit, newdata = newdata, original = TRUE, type = "risk",
                         cause = 1, times = time_point)
  risk_rr <- riskRegression::predictRisk(fit, newdata = newdata, cause = 1, times = time_point)

  expect_equal(as.numeric(risk_burgle), as.numeric(risk_rr), tolerance = 1e-5)
})

test_that("predict.burgle_CauseSpecificCox risk matches riskRegression::predictRisk for cause 2", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  newdata <- head(Melanoma)
  time_point <- 1000

  risk_burgle <- predict(bfit, newdata = newdata, original = TRUE, type = "risk",
                         cause = 2, times = time_point)
  risk_rr <- riskRegression::predictRisk(fit, newdata = newdata, cause = 2, times = time_point)

  expect_equal(as.numeric(risk_burgle), as.numeric(risk_rr), tolerance = 1e-5)
})

test_that("predict.burgle_CauseSpecificCox risk with multiple times has correct dimensions", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  newdata <- head(Melanoma)
  times <- c(500, 1000, 2000)

  result <- predict(bfit, newdata = newdata, original = TRUE, type = "risk",
                    cause = 1, times = times)

  expect_equal(nrow(result), nrow(newdata))
  expect_equal(ncol(result), length(times))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_CauseSpecificCox risk matches riskRegression with multiple times", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  newdata <- head(Melanoma)
  times <- c(500, 1000, 2000)

  risk_burgle <- predict(bfit, newdata = newdata, original = TRUE, type = "risk",
                         cause = 1, times = times)
  risk_rr <- riskRegression::predictRisk(fit, newdata = newdata, cause = 1, times = times)

  expect_equal(dim(risk_burgle), dim(risk_rr))
  expect_equal(as.numeric(risk_burgle), as.numeric(risk_rr), tolerance = 1e-5)
})

## ##############################################################################
## CauseSpecificCox: Single Observation Tests
## ##############################################################################

test_that("predict.burgle_CauseSpecificCox works with a single observation", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = Melanoma[1, ], original = TRUE, type = "risk",
                    cause = 1, times = 1000)

  expect_equal(nrow(result), 1)
  expect_true(all(result >= 0 & result <= 1))
})

## ##############################################################################
## CauseSpecificCox: Multiple Draws Tests
## ##############################################################################

test_that("predict.burgle_CauseSpecificCox multiple draws returns list for risk type", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  set.seed(42)
  result <- predict(bfit, newdata = head(Melanoma), original = FALSE, draws = 3,
                    type = "risk", cause = 1, times = 1000)

  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_true(all(sapply(result, function(x) all(x >= 0 & x <= 1))))
})

test_that("predict.burgle_CauseSpecificCox multiple draws for lp returns list", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  set.seed(42)
  result <- predict(bfit, newdata = head(Melanoma), original = FALSE, draws = 3, type = "lp")

  expect_true(is.list(result))
  expect_equal(length(result), 3)
})

test_that("predict.burgle_CauseSpecificCox multiple draws produce varied predictions", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  set.seed(123)
  result <- predict(bfit, newdata = head(Melanoma), original = FALSE, draws = 50,
                    type = "risk", cause = 1, times = 1000)

  ## Different draws should produce different predictions
  draw_matrix <- do.call(cbind, lapply(result, as.numeric))
  expect_true(all(apply(draw_matrix, 1, sd) > 0))
})

## ##############################################################################
## CauseSpecificCox: Response Type Tests
## ##############################################################################

test_that("predict.burgle_CauseSpecificCox type='response' returns binary outcomes", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  set.seed(42)
  result <- predict(bfit, newdata = head(Melanoma), original = TRUE, type = "response",
                    cause = 1, times = 1000)

  expect_true(all(result %in% c(0, 1)))
  expect_equal(nrow(result), nrow(head(Melanoma)))
})

test_that("predict.burgle_CauseSpecificCox type='response' with sims > 1 returns list of binary matrices", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  set.seed(42)
  result <- predict(bfit, newdata = head(Melanoma), original = TRUE, type = "response",
                    cause = 1, times = 1000, sims = 3)

  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_true(all(sapply(result, function(x) all(x %in% c(0L, 1L)))))
})

## ##############################################################################
## CauseSpecificCox: Error and Warning Tests
## ##############################################################################

test_that("predict.burgle_CauseSpecificCox errors when newdata is not a data.frame", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_error(
    predict(bfit, newdata = list(age = 50, sex = 1), type = "risk", cause = 1, times = 1000),
    "newdata must be an object of class data.frame"
  )
})

test_that("predict.burgle_CauseSpecificCox errors when cause exceeds number of causes", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_error(
    predict(bfit, newdata = head(Melanoma), type = "risk", cause = 99, times = 1000),
    "Invalid cause"
  )
})

test_that("predict.burgle_CauseSpecificCox errors when cause is negative", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_error(
    predict(bfit, newdata = head(Melanoma), type = "risk", cause = -1, times = 1000),
    "Invalid cause"
  )
})

test_that("predict.burgle_CauseSpecificCox errors when cause is non-numeric", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_error(
    predict(bfit, newdata = head(Melanoma), type = "risk", cause = "melanoma", times = 1000),
    "Invalid cause"
  )
})

test_that("predict.burgle_CauseSpecificCox errors when times is missing for risk type", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_error(
    predict(bfit, newdata = head(Melanoma), type = "risk", cause = 1),
    "times is missing"
  )
})

test_that("predict.burgle_CauseSpecificCox warns when times is unsorted", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_warning(
    predict(bfit, newdata = head(Melanoma), type = "risk", cause = 1, times = c(2000, 500)),
    "times is unsorted"
  )
})

test_that("predict.burgle_CauseSpecificCox errors when original=TRUE and draws > 1", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(riskRegression::Hist(time, event) ~ age + sex, data = Melanoma)
  bfit <- burgle(fit)

  expect_error(
    predict(bfit, newdata = head(Melanoma), original = TRUE, draws = 3,
            type = "risk", cause = 1, times = 1000),
    "Can only have one draw from the original model"
  )
})

## ##############################################################################
## CauseSpecificCox: Strata Tests
## ##############################################################################

test_that("burgle.CauseSpecificCox handles strata in model formula", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(
    riskRegression::Hist(time, event) ~ age + survival::strata(sex),
    data = Melanoma
  )
  bfit <- burgle(fit)

  expect_s3_class(bfit, "burgle_CauseSpecificCox")
  expect_true(!is.null(bfit$coef))
})

test_that("predict.burgle_CauseSpecificCox with strata returns valid probabilities", {
  data("Melanoma", package = "riskRegression")
  fit <- riskRegression::CSC(
    riskRegression::Hist(time, event) ~ age + survival::strata(sex),
    data = Melanoma
  )
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(Melanoma), original = TRUE, type = "risk",
                    cause = 1, times = 1000)

  expect_true(all(result >= 0 & result <= 1))
  expect_equal(nrow(result), nrow(head(Melanoma)))
})
