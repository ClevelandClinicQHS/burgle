test_that("burgle_multinom handles weighted subset models with interactions", {
  iris_edge <- iris
  iris_edge$grp <- factor(rep(c("g1", "g2"), length.out = nrow(iris_edge)),
                          levels = c("g1", "g2", "g3"))
  iris_edge$w <- rep(1:3, length.out = nrow(iris_edge))

  fit <- nnet::multinom(
    Species ~ Petal.Width * grp + Sepal.Length,
    data = iris_edge,
    weights = w,
    subset = 1:120,
    trace = FALSE
  )
  bfit <- burgle(fit)
  preds <- predict(bfit, newdata = head(iris_edge), type = "odds")

  expect_equal(dim(preds), c(6, 3))
  expect_true(all(preds >= 0 & preds <= 1))
  expect_true(all(abs(rowSums(preds) - 1) < 1e-8))
})

test_that("burgle_multinom handles no-intercept models with mixed predictors", {
  iris_edge <- iris
  iris_edge$grp <- factor(rep(c("g1", "g2"), length.out = nrow(iris_edge)))

  fit <- nnet::multinom(Species ~ 0 + grp + Petal.Width, data = iris_edge, trace = FALSE)
  bfit <- burgle(fit)
  preds <- predict(bfit, newdata = head(iris_edge), type = "odds")

  expect_equal(dim(preds), c(6, 3))
  expect_true(all(abs(rowSums(preds) - 1) < 1e-8))
})

test_that("burgle_multinom errors on truly new factor levels", {
  iris_edge <- iris
  iris_edge$grp <- factor(rep(c("g1", "g2"), length.out = nrow(iris_edge)))

  fit <- nnet::multinom(Species ~ grp + Petal.Width, data = iris_edge, trace = FALSE)
  bfit <- burgle(fit)

  new_data <- head(iris_edge, 2)
  new_data$grp <- factor(c("g3", "g3"))

  expect_error(predict(bfit, newdata = new_data, type = "odds"), "new level")
})

test_that("burgle_multinom silently drops incomplete rows in newdata", {
  iris_edge <- iris
  iris_edge$grp <- factor(rep(c("g1", "g2"), length.out = nrow(iris_edge)))

  fit <- nnet::multinom(Species ~ grp + Petal.Width, data = iris_edge, trace = FALSE)
  bfit <- burgle(fit)

  new_data <- head(iris_edge, 2)
  new_data$Petal.Width[1] <- NA_real_
  preds <- predict(bfit, newdata = new_data, type = "odds")

  expect_equal(nrow(preds), 1)
  expect_false(anyNA(preds))
})

test_that("multinom offset formulas currently break during fitting", {
  expect_error(
    nnet::multinom(
      Species ~ Petal.Width + offset(Sepal.Width),
      data = iris,
      trace = FALSE
    )
  )
})

test_that("burgle_multinom response simulations work for single-row newdata", {
  iris_edge <- iris
  iris_edge$grp <- factor(rep(c("g1", "g2"), length.out = nrow(iris_edge)))

  fit <- nnet::multinom(Species ~ grp + Petal.Width, data = iris_edge, trace = FALSE)
  bfit <- burgle(fit)
  sims <- predict(bfit, newdata = iris_edge[1, , drop = FALSE], type = "response", sims = 2)

  expect_true(is.list(sims))
  expect_equal(length(sims), 2)
  expect_true(all(vapply(sims, length, integer(1)) == 1L))
})
