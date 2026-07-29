## ##############################################################################
## ranger::ranger - Fast random forest
## ##############################################################################

test_that("burgle.ranger strips training predictions", {
  skip_if_not_installed("ranger")

  set.seed(42)
  fit  <- ranger::ranger(Sepal.Length ~ Sepal.Width + Petal.Length,
                         data = iris, num.trees = 10)
  bfit <- burgle(fit)

  expect_null(bfit$predictions)
  expect_true(inherits(bfit, "burgle_ranger"))
  expect_true(inherits(bfit, "ranger"))
})

test_that("burgle.ranger keeps forest structure", {
  skip_if_not_installed("ranger")

  set.seed(42)
  fit  <- ranger::ranger(Sepal.Length ~ Sepal.Width + Petal.Length,
                         data = iris, num.trees = 10)
  bfit <- burgle(fit)

  expect_false(is.null(bfit$forest))
})

test_that("predict.burgle_ranger regression returns numeric vector", {
  skip_if_not_installed("ranger")

  set.seed(42)
  fit  <- ranger::ranger(Sepal.Length ~ Sepal.Width + Petal.Length,
                         data = iris, num.trees = 10)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), type = "response")

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(head(iris)))
  expect_false(any(is.na(result)))
})

test_that("predict.burgle_ranger classification probability forest", {
  skip_if_not_installed("ranger")

  set.seed(42)
  fit  <- ranger::ranger(Species ~ Sepal.Width + Petal.Length,
                         data = iris, num.trees = 10,
                         probability = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), type = "risk")

  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(head(iris)))
  ## Rows should sum to approximately 1
  expect_true(all(abs(rowSums(result) - 1.0) < 1e-5))
})

test_that("predict.burgle_ranger classification response returns class labels", {
  skip_if_not_installed("ranger")

  set.seed(42)
  fit  <- ranger::ranger(Species ~ Sepal.Width + Petal.Length,
                         data = iris, num.trees = 10,
                         probability = TRUE)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), type = "response")

  expect_true(all(result %in% levels(iris$Species)))
  expect_equal(length(result), nrow(head(iris)))
})

test_that("predict.burgle_ranger regression warns when type='risk'", {
  skip_if_not_installed("ranger")

  set.seed(42)
  fit  <- ranger::ranger(Sepal.Length ~ Sepal.Width + Petal.Length,
                         data = iris, num.trees = 10)
  bfit <- burgle(fit)

  expect_warning(predict(bfit, newdata = head(iris), type = "risk"),
                 "Only 'response' is available")
})

test_that("burgle.ranger reduces object size", {
  skip_if_not_installed("ranger")

  set.seed(42)
  fit  <- ranger::ranger(Sepal.Length ~ Sepal.Width + Petal.Length,
                         data = iris, num.trees = 50)
  bfit <- burgle(fit)

  expect_lt(object.size(bfit), object.size(fit))
})
