xgb_test_fit <- function(x, y, objective = "reg:squarederror", ...){
  set.seed(123)
  params <- c(list(objective = objective, nthread = 1, max_depth = 2,
                   eta = 0.3), list(...))
  if (identical(params$booster, "gblinear")) params$max_depth <- NULL
  xgboost::xgb.train(
    params = params,
    data = xgboost::xgb.DMatrix(x, label = y, nthread = 1),
    nrounds = 5, verbose = 0
  )
}

xgb_expect_parity <- function(fit, newdata, ...){
  expect_equal(predict(burgle(fit), newdata, ...),
               predict(fit, newdata, ...), tolerance = 0)
}

xgb_test_legacy <- function(){
  "as_booster" %in% names(formals(xgboost::xgb.load.raw))
}

test_that("XGBoost regression predictions match on multiple datasets and row counts", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  datasets <- list(
    list(x = as.matrix(mtcars[, c("wt", "hp", "disp")]), y = mtcars$mpg),
    list(x = as.matrix(iris[, 2:4]), y = iris$Sepal.Length),
    list(x = as.matrix(airquality[, c("Solar.R", "Wind", "Temp")]),
         y = ifelse(is.na(airquality$Ozone), 0, airquality$Ozone))
  )
  for (d in datasets) {
    fit <- xgb_test_fit(d$x, d$y)
    for (n in c(1L, 6L, nrow(d$x))) {
      xgb_expect_parity(fit, d$x[seq_len(n), , drop = FALSE])
    }
    xgb_expect_parity(fit, unname(d$x))
  }
})

test_that("XGBoost classification preserves probabilities, labels, and shapes", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(iris[, 1:4])
  fit <- xgb_test_fit(x, as.numeric(iris$Species == "setosa"), "binary:logistic")
  xgb_expect_parity(fit, x)
  xgb_expect_parity(fit, x, outputmargin = TRUE)

  for (objective in c("multi:softprob", "multi:softmax")) {
    fit <- xgb_test_fit(x, as.integer(iris$Species) - 1L, objective, num_class = 3)
    xgb_expect_parity(fit, x)
    xgb_expect_parity(fit, x[1, , drop = FALSE], strict_shape = TRUE)
    xgb_expect_parity(fit, x, outputmargin = TRUE)
  }
})

test_that("XGBoost accepts dense, sparse, and DMatrix prediction inputs", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  skip_if_not_installed("Matrix")
  x <- as.matrix(mtcars[, c("wt", "hp", "am")])
  sparse <- as(Matrix::Matrix(x, sparse = TRUE), "dgCMatrix")
  csr <- as(sparse, "RsparseMatrix")
  inputs <- list(x, sparse, csr, xgboost::xgb.DMatrix(x, nthread = 1),
                 xgboost::xgb.DMatrix(sparse, nthread = 1))
  for (training in list(x, sparse)) {
    fit <- xgb_test_fit(training, mtcars$mpg)
    for (input in inputs) xgb_expect_parity(fit, input)
  }
})

test_that("XGBoost sparse datasets and encoded categorical predictors match", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  e <- new.env()
  utils::data("agaricus.train", package = "xgboost", envir = e)
  x <- e$agaricus.train$data[1:200, ]
  fit <- xgb_test_fit(x, e$agaricus.train$label[1:200], "binary:logistic")
  xgb_expect_parity(fit, x[1:10, , drop = FALSE])
  xgb_expect_parity(fit, x[1, , drop = FALSE])

  x <- stats::model.matrix(~ Species + Sepal.Width - 1, data = iris)
  fit <- xgb_test_fit(x, iris$Sepal.Length)
  xgb_expect_parity(fit, x)
})

test_that("XGBoost missing values, sentinels, and base margins are preserved", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(iris[, 1:4])
  x[c(1, 20, 50), 2] <- NA_real_
  fit <- xgb_test_fit(x, iris$Sepal.Length)
  xgb_expect_parity(fit, x)
  sentinel <- x
  sentinel[is.na(sentinel)] <- -999
  xgb_expect_parity(fit, sentinel, missing = -999)
  dm <- xgboost::xgb.DMatrix(x, base_margin = rep(0.5, nrow(x)), nthread = 1)
  xgb_expect_parity(fit, dm)
})

test_that("XGBoost prediction options are forwarded without changing outputs", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(mtcars[, c("wt", "hp", "disp")])
  fit <- xgb_test_fit(x, mtcars$mpg)
  xgb_expect_parity(fit, x, predleaf = TRUE)
  xgb_expect_parity(fit, x, predcontrib = TRUE)
  xgb_expect_parity(fit, x, predcontrib = TRUE, approxcontrib = TRUE)
  xgb_expect_parity(fit, x[1:2, , drop = FALSE], predinteraction = TRUE)
  xgb_expect_parity(fit, x, iterationrange = c(1, 3))
  xgb_expect_parity(fit, x, strict_shape = TRUE)
})

test_that("XGBoost linear and DART boosters retain deterministic predictions", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(mtcars[, c("wt", "hp")])
  for (booster in c("gblinear", "dart")) {
    fit <- xgb_test_fit(x, mtcars$mpg, booster = booster)
    xgb_expect_parity(fit, x)
    xgb_expect_parity(fit, x, outputmargin = TRUE)
  }
})

test_that("XGBoost count, survival, and ranking objectives retain predictions", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(mtcars[, c("wt", "hp")])
  fit <- xgb_test_fit(x, mtcars$cyl, "count:poisson")
  xgb_expect_parity(fit, x)
  xgb_expect_parity(fit, x, outputmargin = TRUE)

  lung <- survival::lung
  x <- as.matrix(lung[, c("age", "sex", "ph.ecog")])
  y <- ifelse(lung$status == 2, lung$time, -lung$time)
  fit <- xgb_test_fit(x, y, "survival:cox")
  xgb_expect_parity(fit, x)

  x <- as.matrix(iris[, 1:4])
  dm <- xgboost::xgb.DMatrix(x, label = rep(0:2, 50), nthread = 1)
  xgboost::setinfo(dm, "group", rep(5L, 30))
  fit <- xgboost::xgb.train(
    params = list(objective = "rank:pairwise", nthread = 1, max_depth = 2),
    data = dm, nrounds = 5, verbose = 0
  )
  xgb_expect_parity(fit, x)
})

test_that("XGBoost early stopping survives burgling and serialization", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(mtcars[, c("wt", "hp")])
  train <- xgboost::xgb.DMatrix(x, label = mtcars$mpg, nthread = 1)
  valid <- xgboost::xgb.DMatrix(x, label = -mtcars$mpg, nthread = 1)
  args <- list(params = list(objective = "reg:squarederror", eval_metric = "rmse",
                            nthread = 1, max_depth = 2, base_score = 0),
               data = train, nrounds = 30, early_stopping_rounds = 3, verbose = 0)
  eval_arg <- if ("evals" %in% names(formals(xgboost::xgb.train))) "evals" else "watchlist"
  args[[eval_arg]] <- list(valid = valid)
  fit <- do.call(xgboost::xgb.train, args)
  best <- xgboost::xgb.attributes(fit)$best_iteration
  expect_false(is.null(best))
  bfit <- burgle(fit)
  raw <- bfit$raw
  if (!is.null(attr(raw, "compression"))) {
    raw <- memDecompress(raw, type = attr(raw, "compression"))
  }
  expect_equal(xgboost::xgb.attributes(xgboost::xgb.load.raw(raw))$best_iteration,
               best)
  xgb_expect_parity(fit, x)
  expect_equal(predict(unserialize(serialize(bfit, NULL)), x),
               predict(fit, x), tolerance = 0)
})

test_that("XGBoost buffers are losslessly compressed and metadata is prediction-only", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(mtcars[, c("wt", "hp")])
  fit <- xgb_test_fit(x, mtcars$mpg)
  raw <- xgboost::xgb.save.raw(fit, raw_format = "ubj")
  if (xgb_test_legacy()) {
    fit$params$unused <- rep("unnecessary training parameter", 1000)
  } else {
    params <- attr(fit, "params")
    params$unused <- rep("unnecessary training parameter", 1000)
    attr(fit, "params") <- params
  }
  bfit <- burgle(fit)
  expect_identical(attr(bfit$raw, "compression"), "xz")
  expect_identical(memDecompress(bfit$raw, type = "xz"), raw)
  expect_lt(as.numeric(object.size(bfit$raw)), as.numeric(object.size(raw)))
  old_metadata <- if (xgb_test_legacy()) {
    fit[c("feature_names", "params")]
  } else {
    attributes(fit)[intersect(c("metadata", "params"), names(attributes(fit)))]
  }
  old <- structure(list(raw = raw, metadata = old_metadata,
                        model_class = "xgb.Booster"), class = "burgle_xgboost")
  expect_lt(as.numeric(object.size(bfit)), as.numeric(object.size(old)))
  expect_false("unused" %in% names(bfit$metadata$params))
  if (xgb_test_legacy()) {
    expect_setequal(names(bfit$metadata$params), "nthread")
    expect_identical(bfit$metadata$feature_names, colnames(x))
  } else {
    expect_length(bfit$metadata, 0)
  }
  expect_equal(predict(bfit, x), predict(fit, x), tolerance = 0)
  expect_equal(predict(old, x), predict(fit, x), tolerance = 0)
})

test_that("Minimal XGBoost metadata retains linear booster tree-limit behavior", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(mtcars[, c("wt", "hp")])
  fit <- xgb_test_fit(x, mtcars$mpg, booster = "gblinear")
  if (xgb_test_legacy()) {
    expect_identical(burgle(fit)$metadata$params$booster, "gblinear")
    xgb_expect_parity(fit, x, ntreelimit = 1)
  } else {
    xgb_expect_parity(fit, x)
  }
})

test_that("Minimal high-level XGBoost metadata preserves quantile output names", {
  skip_if_not_installed("xgboost", minimum_version = "3.0.0")
  x <- as.matrix(mtcars[, c("wt", "hp")])
  fit <- xgboost::xgboost(x = x, y = mtcars$mpg, nrounds = 5,
                         objective = "reg:quantileerror", quantile_alpha = c(0.1, 0.9),
                         nthreads = 1, verbosity = 0)
  bfit <- burgle(fit)
  expect_identical(bfit$metadata$params$quantile_alpha, c(0.1, 0.9))
  expect_setequal(names(bfit$metadata$params), "quantile_alpha")
  expect_equal(predict(bfit, x), predict(fit, x), tolerance = 0)
  expect_equal(predict(unserialize(serialize(bfit, NULL)), x),
               predict(fit, x), tolerance = 0)
})

test_that("Minimal high-level XGBoost metadata preserves multiple response names", {
  skip_if_not_installed("xgboost", minimum_version = "3.0.0")
  x <- as.matrix(iris[, 1:2])
  y <- as.matrix(iris[, 3:4])
  fit <- xgboost::xgboost(x = x, y = y, nrounds = 5,
                         nthreads = 1, verbosity = 0)
  bfit <- burgle(fit)
  expect_identical(bfit$metadata$metadata$y_names, colnames(y))
  expect_equal(predict(bfit, x), predict(fit, x), tolerance = 0)
  old <- bfit
  old$raw <- xgboost::xgb.save.raw(fit, raw_format = "ubj")
  old$metadata <- attributes(fit)[c("metadata", "params")]
  expect_equal(predict(old, x), predict(fit, x), tolerance = 0)
})

test_that("Burgled XGBoost objects omit training artifacts and are RDS-safe", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(mtcars[, c("wt", "hp")])
  fit <- xgb_test_fit(x, mtcars$mpg)
  if (xgb_test_legacy()) {
    fit$training_data <- x
    fit$callbacks <- list(function() x)
  } else {
    attr(fit, "training_data") <- x
    attr(fit, "callbacks") <- list(function() x)
  }
  bfit <- burgle(fit)
  expect_s3_class(bfit, "burgle_xgboost")
  expect_type(bfit$raw, "raw")
  expect_named(bfit, c("raw", "metadata", "model_class"))
  expect_false(any(c("training_data", "callbacks", "evaluation_log", "call") %in%
                     names(bfit$metadata)))
  path <- tempfile(fileext = ".rds")
  saveRDS(bfit, path)
  restored <- readRDS(path)
  unlink(path)
  expect_equal(predict(restored, x), predict(fit, x), tolerance = 0)
  expect_equal(burgle(fit), bfit)
  expect_equal(predict(bfit, x), predict(bfit, x), tolerance = 0)
  restored_fit <- unserialize(serialize(fit, NULL))
  xgb_expect_parity(restored_fit, x)
})

test_that("XGBoost feature validation and invalid input errors are retained", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(mtcars[, c("wt", "hp")])
  fit <- xgb_test_fit(x, mtcars$mpg)
  bfit <- burgle(fit)
  expect_error(predict(bfit, x[, 1, drop = FALSE]))
  expect_error(predict(bfit, list(wt = 1, hp = 2)))
  if (xgb_test_legacy()) {
    expect_error(predict(bfit, x[, 2:1]), "Feature names")
    expect_error(predict(bfit, as.data.frame(x)))
  } else {
    xgb_expect_parity(fit, x[, 2:1], validate_features = TRUE)
    xgb_expect_parity(fit, as.data.frame(x))
  }
})

test_that("High-level xgboost models retain their prediction interface", {
  skip_if_not_installed("xgboost", minimum_version = "1.7.0")
  x <- as.matrix(iris[, 1:4])
  if ("x" %in% names(formals(xgboost::xgboost))) {
    for (y in list(iris$Sepal.Length, iris$Species,
                   factor(iris$Species == "setosa"))) {
      fit <- xgboost::xgboost(x = x, y = y, nrounds = 5, max_depth = 2,
                             nthreads = 1, verbosity = 0)
      xgb_expect_parity(fit, x)
      xgb_expect_parity(fit, as.data.frame(x)[, 4:1])
      xgb_expect_parity(fit, x, iteration_range = c(1, 3))
      restored <- unserialize(serialize(burgle(fit), NULL))
      expect_equal(predict(restored, x), predict(fit, x), tolerance = 0)
      expect_error(predict(burgle(fit), xgboost::xgb.DMatrix(x, nthread = 1)),
                   "not supported")
      for (type in c("raw", "leaf", "contrib")) {
        xgb_expect_parity(fit, x, type = type)
      }
      if (is.factor(y)) {
        xgb_expect_parity(fit, x, type = "class")
        expect_equal(predict(restored, x, type = "class"),
                     predict(fit, x, type = "class"), tolerance = 0)
      }
    }

    df <- data.frame(length = iris$Sepal.Length, species = iris$Species)
    fit <- xgboost::xgboost(x = df, y = iris$Sepal.Width,
                           nrounds = 5, nthreads = 1, verbosity = 0)
    xgb_expect_parity(fit, df)
  } else {
    fit <- xgboost::xgboost(data = x, label = iris$Sepal.Length, nrounds = 5,
                           objective = "reg:squarederror", nthread = 1, verbose = 0)
    xgb_expect_parity(fit, x)
  }
})
