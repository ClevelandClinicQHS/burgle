## ##############################################################################
## randomForest::randomForest - Random forest
## ##############################################################################

test_that("burgle.randomForest strips training data components", {
  skip_if_not_installed("randomForest")

  set.seed(42)
  fit  <- randomForest::randomForest(Sepal.Length ~ Sepal.Width + Petal.Length,
                                     data = iris, ntree = 10)
  bfit <- burgle(fit)

  expect_null(bfit$y)
  expect_null(bfit$predicted)
  expect_null(bfit$votes)
  expect_null(bfit$oob.times)
})

test_that("burgle.randomForest keeps forest structure", {
  skip_if_not_installed("randomForest")

  set.seed(42)
  fit  <- randomForest::randomForest(Sepal.Length ~ Sepal.Width + Petal.Length,
                                     data = iris, ntree = 10)
  bfit <- burgle(fit)

  expect_false(is.null(bfit$forest))
  expect_true(inherits(bfit, "burgle_randomForest"))
  expect_true(inherits(bfit, "randomForest"))
})

test_that("burgle.randomForest errors without forest", {
  skip_if_not_installed("randomForest")

  set.seed(42)
  fit  <- randomForest::randomForest(Sepal.Length ~ Sepal.Width + Petal.Length,
                                     data = iris, ntree = 10,
                                     keep.forest = FALSE)
  expect_error(burgle(fit), "No forest found")
})

test_that("predict.burgle_randomForest regression returns numeric", {
  skip_if_not_installed("randomForest")

  set.seed(42)
  fit  <- randomForest::randomForest(Sepal.Length ~ Sepal.Width + Petal.Length,
                                     data = iris, ntree = 10)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), type = "response")

  expect_true(is.numeric(result))
  expect_equal(length(result), nrow(head(iris)))
  expect_false(any(is.na(result)))
})

test_that("predict.burgle_randomForest classification risk returns probability matrix", {
  skip_if_not_installed("randomForest")

  set.seed(42)
  fit  <- randomForest::randomForest(Species ~ Sepal.Width + Petal.Length,
                                     data = iris, ntree = 10)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), type = "risk")

  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(head(iris)))
  expect_equal(ncol(result), length(levels(iris$Species)))
  ## Probabilities should sum to 1
  expect_true(all(abs(rowSums(result) - 1.0) < 1e-8))
})

test_that("predict.burgle_randomForest classification response returns class labels", {
  skip_if_not_installed("randomForest")

  set.seed(42)
  fit  <- randomForest::randomForest(Species ~ Sepal.Width + Petal.Length,
                                     data = iris, ntree = 10)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), type = "response")

  expect_true(all(result %in% levels(iris$Species)))
  expect_equal(length(result), nrow(head(iris)))
})

test_that("predict.burgle_randomForest classification sims > 1 returns list", {
  skip_if_not_installed("randomForest")

  set.seed(42)
  fit  <- randomForest::randomForest(Species ~ Sepal.Width + Petal.Length,
                                     data = iris, ntree = 10)
  bfit <- burgle(fit)

  result <- predict(bfit, newdata = head(iris), type = "response", sims = 3)

  expect_true(is.list(result))
  expect_equal(length(result), 3)
})

test_that("burgle.randomForest reduces object size", {
  skip_if_not_installed("randomForest")

  set.seed(42)
  fit  <- randomForest::randomForest(Sepal.Length ~ Sepal.Width + Petal.Length,
                                     data = iris, ntree = 50)
  bfit <- burgle(fit)

  expect_lt(object.size(bfit), object.size(fit))
})
