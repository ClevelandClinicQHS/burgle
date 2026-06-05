
test_that("predict.burgle_rfsrc regression returns numeric", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  fit <- randomForestSRC::rfsrc(Sepal.Length ~ Sepal.Width + Petal.Length,
                                data = iris,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), type = "response")

  ## "Should return numeric vector
  expect_true(is.numeric(result))

  ## "Should have correct length
  expect_equal(length(result), nrow(head(iris)))

  ## "Should not have NA values
  expect_false(any(is.na(result)))
})

test_that("predict.burgle_rfsrc regression with type='risk' gives warning", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  fit <- randomForestSRC::rfsrc(Sepal.Length ~ Sepal.Width + Petal.Length,
                                data = iris,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  ## "Should warn that only response is available
  expect_warning(predict(bfit, newdata = head(iris), type = "risk"),
                 "Only response is available")
})

test_that("predict.burgle_rfsrc classification returns probabilities for type='risk'", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  iris_cat <- iris
  iris_cat$Species_binary <- as.factor(iris_cat$Species %in% c("setosa", "versicolor"))

  fit <- randomForestSRC::rfsrc(Species_binary ~ Sepal.Width + Petal.Length,
                                data = iris_cat,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris_cat), type = "risk")

  ## "Should return matrix of probabilities
  expect_true(is.matrix(result))

  ## "Should have correct dimensions
  expect_equal(nrow(result), nrow(head(iris_cat)))

  ## "Probabilities should sum to 1
  expect_true(all(abs(rowSums(result) - 1) < 1e-10))
})

test_that("predict.burgle_rfsrc classification returns samples for type='response'", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  iris_cat <- iris
  iris_cat$Species_binary <- as.factor(iris_cat$Species %in% c("setosa", "versicolor"))

  fit <- randomForestSRC::rfsrc(Species_binary ~ Sepal.Width + Petal.Length,
                                data = iris_cat,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris_cat), type = "response")

  ## "Should return character vector
  expect_true(is.character(result))

  ## "Should have correct length
  expect_equal(length(result), nrow(head(iris_cat)))

  ## "All values should be valid class labels
  expect_true(all(result %in% c("FALSE", "TRUE")))
})

test_that("predict.burgle_rfsrc classification with sims > 1", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  iris_cat <- iris
  iris_cat$Species_binary <- as.factor(iris_cat$Species %in% c("setosa", "versicolor"))

  fit <- randomForestSRC::rfsrc(Species_binary ~ Sepal.Width + Petal.Length,
                                data = iris_cat,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris_cat), type = "response", sims = 3)

  ## "Should return list
  expect_true(is.list(result))

  ## "Should have length equal to sims
  expect_equal(length(result), 3)

  ## "Each element should be character vector
  expect_true(all(sapply(result, is.character)))
})

### something is not right here
test_that("predict.burgle_rfsrc survival returns risk", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  lung_data <- survival::lung
  lung_data$status <- lung_data$status - 1
  # lung_data$surv_obj <- survival::Surv(lung_data$time, lung_data$status)

  fit <- randomForestSRC::rfsrc(Surv(time, status) ~ age + sex,
                                data = lung_data,
                                forest = TRUE,

                                ntree = 10)

  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung_data),
                    type = "risk", times = c(500, 1000))

  ## "Should return matrix
  expect_true(is.matrix(result))

  ## "Should have correct number of rows
  expect_equal(nrow(result), nrow(head(lung_data)))

  ## "Should have correct number of time points
  expect_equal(ncol(result), 2)

  ## "All values should be between 0 and 1
  expect_true(all(result >= 0 & result <= 1))
})

test_that("predict.burgle_rfsrc survival type='response' with sims", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  lung_data <- survival::lung
  lung_data$status <- lung_data$status - 1

  fit <- randomForestSRC::rfsrc(Surv(time, status) ~ age + sex,
                                data = lung_data,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(lung_data),
                    type = "response", times = c(500, 1000), sims = 2)

  ## "Should return list
  expect_true(is.list(result))

  ## "Should have length equal to sims
  expect_equal(length(result), 2)

  ## "Each element should be matrix
  expect_true(all(sapply(result, is.matrix)))
})

test_that("predict.burgle_rfsrc competing risk requires cause and times", {
  skip_if_not_installed("randomForestSRC")

  ## "Note: Creating competing risk data requires specific setup
  ## "This is a basic test structure
  set.seed(123)
  lung_data <- survival::lung
  lung_data$status <- lung_data$status - 1

  fit <- randomForestSRC::rfsrc(Surv(time, status) ~ age + sex,
                                data = lung_data,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  ## "Should work with both cause and times specified
  result <- predict(bfit, newdata = head(lung_data),
                    type = "risk", times = 500, cause = 1)

  expect_true(is.matrix(result))
})

test_that("predict.burgle_rfsrc different sample sizes", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  fit <- randomForestSRC::rfsrc(Sepal.Length ~ Sepal.Width + Petal.Length,
                                data = iris,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  ## "Test with 1 observation
  result1 <- predict(bfit, newdata = iris[1, ], type = "response")
  expect_equal(length(result1), 1)

  ## "Test with multiple observations
  result_multi <- predict(bfit, newdata = head(iris, 10), type = "response")
  expect_equal(length(result_multi), 10)

  ## "Test with all observations
  result_all <- predict(bfit, newdata = iris, , type = "response")
  expect_equal(length(result_all), nrow(iris))
})

test_that("predict.burgle_rfsrc removes burgle_rfsrc class before predict", {
  skip_if_not_installed("randomForestSRC")

  set.seed(123)
  fit <- randomForestSRC::rfsrc(Sepal.Length ~ Sepal.Width + Petal.Length,
                                data = iris,
                                forest = TRUE,
                                ntree = 10)

  bfit <- burgle(fit)

  ## "Verify burgle_rfsrc class is present
  expect_true("burgle_rfsrc" %in% class(bfit))

  ## "After predict, should not error
  result <- predict(bfit, newdata = head(iris), type = "response")
  expect_true(!is.null(result))
})
