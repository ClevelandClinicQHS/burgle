## Actual engine predictions are the oracle, not replacement prediction methods.
workflow_test_dependencies <- function() {
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
}

workflow_test_fit <- function(recipe, specification, data, ...) {
  workflow <- workflows::add_recipe(workflows::workflow(), recipe)
  workflow <- workflows::add_model(workflow, specification, ...)
  generics::fit(workflow, data = data)
}

workflow_test_baked <- function(workflow, newdata) {
  recipe <- workflows::extract_recipe(workflow, estimated = TRUE)
  data <- as.data.frame(recipes::bake(recipe, new_data = newdata,
                                    recipes::all_predictors()))
  mold <- workflows::extract_mold(workflow)
  if (isTRUE(mold$blueprint$intercept)) data$.intercept <- rep(1, nrow(data))
  data[, names(mold$predictors), drop = FALSE]
}

workflow_test_oracle <- function(workflow, newdata, ...) {
  model <- burgle(workflows::extract_fit_engine(workflow))
  ## The existing lm reducer retains response terms; predictors do not.
  if (inherits(model$terms, "terms")) {
    model$terms <- stats::delete.response(model$terms)
  }
  predict(model, newdata = workflow_test_baked(workflow, newdata), ...)
}

workflow_test_compare <- function(workflow, newdata, ...) {
  expect_equal(predict(burgle(workflow), newdata = newdata, ...),
               workflow_test_oracle(workflow, newdata, ...),
               tolerance = 1e-10)
}

## Engines without a tidymodels adapter still share the same compiled interface.
workflow_test_wrap_engine <- function(fit, recipe, predictors) {
  new_burgle_workflow(burgle(fit), compile_workflow_recipe(recipe), predictors)
}

test_that("linear workflows preserve predictions and final predictor names", {
  workflow_test_dependencies()
  recipe <- recipes::recipe(mpg ~ wt + hp + cyl, data = mtcars)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  workflow <- workflow_test_fit(
    recipe, parsnip::set_engine(parsnip::linear_reg(), "lm"), mtcars
  )
  reduced <- burgle(workflow)
  newdata <- mtcars[c(2, 7, 11, 18), c("hp", "cyl", "wt")]
  expect_s3_class(reduced, "burgle_workflow")
  expect_s3_class(reduced$model, "burgle_lm")
  expect_identical(reduced$predictors,
                   names(workflow_test_baked(workflow, newdata)))
  expect_equal(as.numeric(predict(reduced, newdata)),
               predict(workflow, new_data = newdata)$.pred,
               tolerance = 1e-10)
  workflow_test_compare(workflow, newdata)
  expect_equal(predict(reduced, newdata[1, , drop = FALSE]),
               workflow_test_oracle(workflow, newdata[1, , drop = FALSE]),
               tolerance = 1e-10)
})

test_that("linear workflow delegation preserves simulation arguments", {
  workflow_test_dependencies()
  recipe <- recipes::recipe(mpg ~ wt + hp, data = mtcars)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  workflow <- workflow_test_fit(
    recipe, parsnip::set_engine(parsnip::linear_reg(), "lm"), mtcars
  )
  newdata <- head(mtcars[c("wt", "hp")])
  workflow_test_compare(workflow, newdata, original = FALSE, draws = 3,
                        type = "lp", seed = 143)
  for (se_type in c("prediction", "confidence")) {
    workflow_test_compare(workflow, newdata, original = FALSE, draws = 2,
                          sims = 3, type = "response", se = TRUE,
                          se_type = se_type, seed = 144)
    ## The existing limited-response sampler accepts a scalar standard error.
    workflow_test_compare(workflow, newdata[1, , drop = FALSE],
                          original = FALSE, draws = 2, sims = 3,
                          type = "response", se = TRUE, se_type = se_type,
                          limits = c(0, 60), seed = 145)
  }
  expect_error(predict(burgle(workflow), newdata, original = TRUE, draws = 2),
               "one draw")
})

test_that("binomial workflows match probabilities and preserve simulation", {
  workflow_test_dependencies()
  data <- mtcars[c("am", "wt", "hp")]
  data$am <- factor(data$am, levels = c(0, 1))
  recipe <- recipes::recipe(am ~ ., data = data)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  workflow <- workflow_test_fit(
    recipe, parsnip::set_engine(parsnip::logistic_reg(), "glm"), data
  )
  newdata <- head(data[c("wt", "hp")])
  reduced <- burgle(workflow)
  expect_s3_class(reduced$model, "burgle_glm")
  probabilities <- predict(workflow, new_data = newdata, type = "prob")
  expect_equal(stats::plogis(as.numeric(predict(reduced, newdata, type = "lp"))),
               probabilities$.pred_1, tolerance = 1e-10)
  workflow_test_compare(workflow, newdata, type = "link")
  workflow_test_compare(workflow, newdata, original = FALSE, draws = 3,
                        type = "lp", seed = 151)
  workflow_test_compare(workflow, newdata, original = FALSE, draws = 2,
                        sims = 3, type = "response", seed = 152)
})

test_that("multinomial workflows match class probabilities and seeded draws", {
  workflow_test_dependencies()
  skip_if_not_installed("nnet")
  recipe <- recipes::recipe(Species ~ Sepal.Length + Sepal.Width, data = iris)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  specification <- parsnip::set_engine(parsnip::multinom_reg(), "nnet",
                                      trace = FALSE, maxit = 200, Hess = TRUE)
  workflow <- workflow_test_fit(recipe, specification, iris)
  newdata <- iris[c(1, 60, 110), c("Sepal.Length", "Sepal.Width")]
  reduced <- burgle(workflow)
  expect_s3_class(reduced$model, "burgle_multinom")
  expect_equal(unname(predict(reduced, newdata, type = "odds")),
               unname(as.matrix(predict(workflow, new_data = newdata,
                                        type = "prob"))),
               tolerance = 1e-8)
  workflow_test_compare(workflow, newdata, type = "lp")
  workflow_test_compare(workflow, newdata, original = FALSE, draws = 3,
                        type = "odds", floor = TRUE, seed = 161)
  workflow_test_compare(workflow, newdata, original = FALSE, draws = 2,
                        sims = 3, type = "response", floor = TRUE, seed = 162)
})

test_that("zero-step workflows keep factor contrasts and need no outcomes", {
  workflow_test_dependencies()
  data <- mtcars
  data$cyl <- factor(data$cyl)
  recipe <- recipes::recipe(mpg ~ wt + cyl, data = data)
  workflow <- workflow_test_fit(
    recipe, parsnip::set_engine(parsnip::linear_reg(), "lm"), data
  )
  newdata <- head(data[c("cyl", "wt")])
  workflow_test_compare(workflow, newdata)
  expect_equal(as.numeric(predict(burgle(workflow), newdata)),
               predict(workflow, new_data = newdata)$.pred,
               tolerance = 1e-10)
  expect_error(predict(burgle(workflow), newdata["wt"]), "cyl")
  novel <- newdata
  novel$cyl <- factor(rep("12", nrow(novel)))
  expect_error(predict(burgle(workflow), novel), "level|novel|unknown")
})

test_that("non-syntactic names and no-intercept workflow models are retained", {
  workflow_test_dependencies()
  data <- data.frame("fuel economy" = mtcars$mpg,
                     "car weight" = mtcars$wt, horsepower = mtcars$hp,
                     check.names = FALSE)
  recipe <- recipes::recipe(`fuel economy` ~ ., data = data)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  workflow <- workflow_test_fit(
    recipe, parsnip::set_engine(parsnip::linear_reg(), "lm"), data,
    formula = `fuel economy` ~ 0 + `car weight` + horsepower
  )
  newdata <- head(data[c("horsepower", "car weight")])
  workflow_test_compare(workflow, newdata)
  expect_equal(as.numeric(predict(burgle(workflow), newdata)),
               predict(workflow, new_data = newdata)$.pred,
               tolerance = 1e-10)
})

test_that("recipe blueprint intercepts preserve fitted columns and parameters", {
  workflow_test_dependencies()
  skip_if_not_installed("hardhat")
  recipe <- recipes::recipe(mpg ~ wt + hp, data = mtcars)
  recipe <- recipes::step_log(recipe, wt)
  workflow <- workflows::add_recipe(
    workflows::workflow(), recipe,
    blueprint = hardhat::default_recipe_blueprint(intercept = TRUE)
  )
  workflow <- workflows::add_model(
    workflow, parsnip::set_engine(parsnip::linear_reg(), "lm"),
    formula = mpg ~ 0 + .intercept + wt + hp
  )
  workflow <- generics::fit(workflow, data = mtcars)
  reduced <- burgle(workflow)
  engine <- workflows::extract_fit_engine(workflow)
  expect_true(reduced$intercept)
  expect_identical(reduced$predictors,
                   names(workflows::extract_mold(workflow)$predictors))
  expect_identical(reduced$model$coef, stats::coef(engine))
  expect_identical(reduced$model$cov, stats::vcov(engine))
  newdata <- head(mtcars[c("hp", "wt")])
  workflow_test_compare(workflow, newdata)
  expect_equal(as.numeric(predict(reduced, newdata)),
               predict(workflow, new_data = newdata)$.pred,
               tolerance = 1e-10)
})

test_that("aliased workflow coefficients retain underlying burgle behavior", {
  workflow_test_dependencies()
  data <- mtcars[c("mpg", "wt")]
  data$duplicate <- data$wt
  workflow <- workflow_test_fit(
    recipes::recipe(mpg ~ ., data = data),
    parsnip::set_engine(parsnip::linear_reg(), "lm"), data
  )
  newdata <- head(data[c("wt", "duplicate")])
  expect_warning(actual <- predict(burgle(workflow), newdata), "NA")
  expect_warning(expected <- workflow_test_oracle(workflow, newdata), "NA")
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("compiled workflows serialize without recipes or training objects", {
  workflow_test_dependencies()
  data <- mtcars
  data$cyl <- factor(data$cyl)
  recipe <- recipes::recipe(mpg ~ wt + cyl, data = data)
  recipe <- recipes::step_poly(recipe, wt, degree = 2)
  workflow <- workflow_test_fit(
    recipe, parsnip::set_engine(parsnip::linear_reg(), "lm"), data
  )
  reduced <- burgle(workflow)
  inspect <- function(object) {
    expect_false(inherits(object, c("recipe", "workflow", "model_fit",
                                   "data.frame")))
    expect_false(is.environment(object))
    expect_null(attr(object, ".Environment"))
    if (is.list(object)) {
      expect_false(any(names(object) %in%
                         c("recipe", "workflow", "mold", "blueprint",
                           "training", "training_data", "orig_data")))
      invisible(lapply(object, inspect))
    }
  }
  inspect(reduced)
  expect_lt(as.numeric(object.size(reduced)), as.numeric(object.size(workflow)))
  restored <- unserialize(serialize(reduced, NULL))
  newdata <- head(data[c("wt", "cyl")])
  expected <- workflow_test_oracle(workflow, newdata)
  rm(workflow, recipe, data)
  ## Prediction must execute compiled operations, never call recipes::bake().
  local_mocked_bindings(
    bake = function(...) stop("bake must not be used during prediction"),
    .package = "recipes"
  )
  expect_equal(predict(restored, newdata), expected, tolerance = 1e-10)
})

test_that("unfitted workflows and workflows without recipes fail clearly", {
  workflow_test_dependencies()
  specification <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  workflow <- workflows::add_model(workflows::workflow(), specification)
  workflow <- workflows::add_recipe(
    workflow, recipes::recipe(mpg ~ wt, data = mtcars)
  )
  expect_error(burgle(workflow), "fit|train")
  formula_workflow <- workflows::add_model(workflows::workflow(), specification)
  formula_workflow <- workflows::add_formula(formula_workflow, mpg ~ wt)
  formula_workflow <- generics::fit(formula_workflow, data = mtcars)
  expect_error(burgle(formula_workflow), "recipe")
})

test_that("unsupported real engines and matrix blueprints are rejected", {
  workflow_test_dependencies()
  skip_if_not_installed("hardhat")
  skip_if_not_installed("rpart")
  specification <- parsnip::set_engine(parsnip::decision_tree(), "rpart")
  specification <- parsnip::set_mode(specification, "regression")
  workflow <- workflow_test_fit(
    recipes::recipe(mpg ~ wt + hp, data = mtcars), specification, mtcars
  )
  expect_error(burgle(workflow), "supported burgle|engine")
  workflow <- workflows::add_model(
    workflows::workflow(), parsnip::set_engine(parsnip::linear_reg(), "lm")
  )
  workflow <- workflows::add_recipe(
    workflow, recipes::recipe(mpg ~ wt + hp, data = mtcars),
    blueprint = hardhat::default_recipe_blueprint(composition = "matrix")
  )
  workflow <- generics::fit(workflow, data = mtcars)
  expect_error(burgle(workflow), "matrix|data frame")
})

test_that("harmless unsupported steps warn but allow workflow prediction", {
  workflow_test_dependencies()
  recipe <- recipes::recipe(mpg ~ wt + hp, data = mtcars)
  recipe <- recipes::step_zv(recipe, recipes::all_predictors())
  recipe <- recipes::step_log(recipe, wt)
  workflow <- workflow_test_fit(
    recipe, parsnip::set_engine(parsnip::linear_reg(), "lm"), mtcars
  )
  expect_warning(reduced <- burgle(workflow), "step_zv.*skip")
  newdata <- head(mtcars[c("wt", "hp")])
  expect_equal(predict(reduced, newdata),
               workflow_test_oracle(workflow, newdata), tolerance = 1e-10)
})

test_that("omitted step dependencies require externally supplied inputs", {
  workflow_test_dependencies()
  recipe <- recipes::recipe(mpg ~ wt, data = mtcars)
  recipe <- recipes::step_mutate(recipe, wt_copy = wt^2)
  recipe <- recipes::step_log(recipe, wt_copy)
  workflow <- workflow_test_fit(
    recipe, parsnip::set_engine(parsnip::linear_reg(), "lm"), mtcars
  )
  expect_warning(reduced <- burgle(workflow), "step_mutate.*skip")
  newdata <- head(mtcars["wt"])
  expect_error(predict(reduced, newdata), "wt_copy")
  supplied <- newdata
  supplied$wt_copy <- supplied$wt^2
  expect_equal(predict(reduced, supplied),
               workflow_test_oracle(workflow, newdata), tolerance = 1e-10)
})

workflow_test_survival_data <- function() {
  data <- survival::lung[c("time", "status", "age", "sex")]
  data$event <- survival::Surv(data$time, data$status - 1)
  data[c("event", "age", "sex")]
}

test_that("censored Cox workflows preserve risk, linear predictors and times", {
  workflow_test_dependencies()
  skip_if_not_installed("censored")
  loadNamespace("censored")
  data <- workflow_test_survival_data()
  recipe <- recipes::recipe(event ~ age + sex, data = data)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  specification <- parsnip::set_engine(parsnip::proportional_hazards(), "survival")
  workflow <- workflow_test_fit(recipe, specification, data)
  reduced <- burgle(workflow)
  newdata <- head(data[c("age", "sex")])
  expect_s3_class(reduced$model, "burgle_coxph")
  workflow_test_compare(workflow, newdata, type = "lp")
  workflow_test_compare(workflow, newdata, type = "risk", times = c(100, 500))
  workflow_test_compare(workflow, newdata, type = "risk", times = 300,
                        original = FALSE, draws = 2, seed = 171)
  ## Parsnip orients survival linear predictors toward longer survival.
  expect_equal(as.numeric(predict(reduced, newdata, type = "lp")),
               -predict(workflow, new_data = newdata,
                        type = "linear_pred")$.pred_linear_pred,
               tolerance = 1e-8)
  engine <- workflows::extract_fit_engine(workflow)
  expect_equal(unname(predict(reduced, newdata, type = "risk",
                             times = c(100, 500))),
               unname(riskRegression::predictRisk(
                 engine, newdata = workflow_test_baked(workflow, newdata),
                 times = c(100, 500))), tolerance = 1e-8)
  set.seed(172)
  actual <- predict_time(reduced, newdata, seed = 173)
  set.seed(172)
  expected <- predict_time(burgle(engine),
                           workflow_test_baked(workflow, newdata), seed = 173)
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("censored flexsurv workflows preserve risk and sampled times", {
  workflow_test_dependencies()
  skip_if_not_installed("censored")
  skip_if_not_installed("flexsurv")
  loadNamespace("censored")
  data <- workflow_test_survival_data()
  recipe <- recipes::recipe(event ~ age + sex, data = data)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  specification <- parsnip::set_engine(
    parsnip::survival_reg(dist = "weibull"), "flexsurv"
  )
  workflow <- workflow_test_fit(recipe, specification, data)
  newdata <- head(data[c("age", "sex")])
  reduced <- burgle(workflow)
  expect_s3_class(reduced$model, "burgle_flexsurvreg")
  workflow_test_compare(workflow, newdata, type = "lp")
  workflow_test_compare(workflow, newdata, type = "risk", times = c(100, 500))
  workflow_test_compare(workflow, newdata, type = "risk", times = 300,
                        original = FALSE, draws = 2, seed = 181)
  survival <- predict(workflow, new_data = newdata, type = "survival",
                      eval_time = c(100, 500))
  survival <- t(vapply(survival$.pred, function(x) x$.pred_survival, numeric(2)))
  expect_equal(unname(predict(reduced, newdata, type = "risk",
                             times = c(100, 500))),
               unname(1 - survival), tolerance = 1e-8)
  set.seed(182)
  actual <- predict_time(reduced, newdata, seed = 183)
  set.seed(182)
  expected <- predict_time(reduced$model,
                           workflow_test_baked(workflow, newdata), seed = 183)
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("rms cph integrates with compiled recipe predictors", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("rms")
  data <- workflow_test_survival_data()
  recipe <- recipes::recipe(event ~ age + sex, data = data)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  trained <- recipes::prep(recipe)
  baked <- as.data.frame(recipes::juice(trained))
  fit <- rms::cph(event ~ age + sex, data = baked, x = TRUE, y = TRUE,
                  surv = TRUE)
  reduced <- workflow_test_wrap_engine(fit, trained, c("age", "sex"))
  newdata <- head(data[c("age", "sex")])
  baked_new <- as.data.frame(recipes::bake(trained, new_data = newdata,
                                          recipes::all_predictors()))
  for (type in c("lp", "risk")) {
    expect_equal(predict(reduced, newdata, type = type, times = c(100, 500)),
                 predict(burgle(fit), baked_new, type = type,
                         times = c(100, 500)), tolerance = 1e-10)
  }
})

test_that("competing-risk Cox integrates with compiled recipe predictors", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("prodlim")
  set.seed(191)
  data <- data.frame(time = rexp(100, .01),
                     cause = rep(0:2, length.out = 100),
                     age = rnorm(100, 60, 8), sex = rep(1:2, 50))
  recipe <- recipes::recipe(~ age + sex, data = data)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  trained <- recipes::prep(recipe)
  baked <- as.data.frame(recipes::juice(trained))
  baked$time <- data$time
  baked$cause <- data$cause
  fit <- riskRegression::CSC(prodlim::Hist(time, cause) ~ age + sex, data = baked)
  reduced <- workflow_test_wrap_engine(fit, trained, c("age", "sex"))
  newdata <- head(data[c("age", "sex")])
  baked_new <- as.data.frame(recipes::bake(trained, new_data = newdata))
  for (cause in c(1, 2)) {
    expect_equal(predict(reduced, newdata, type = "risk", cause = cause,
                         times = c(100, 500)),
                 predict(burgle(fit), baked_new, type = "risk", cause = cause,
                         times = c(100, 500)), tolerance = 1e-10)
  }
  set.seed(192)
  actual <- predict_time(reduced, newdata)
  set.seed(192)
  expected <- predict_time(burgle(fit), baked_new)
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("randomForestSRC integrates with compiled recipe predictors", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("randomForestSRC")
  recipe <- recipes::recipe(mpg ~ wt + hp, data = mtcars)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  trained <- recipes::prep(recipe)
  baked <- as.data.frame(recipes::juice(trained))
  set.seed(201)
  fit <- randomForestSRC::rfsrc(mpg ~ wt + hp, data = baked, ntree = 10)
  reduced <- workflow_test_wrap_engine(fit, trained, c("wt", "hp"))
  newdata <- head(mtcars[c("wt", "hp")])
  baked_new <- as.data.frame(recipes::bake(trained, new_data = newdata,
                                          recipes::all_predictors()))
  expect_equal(predict(reduced, newdata, type = "response"),
               predict(burgle(fit), baked_new, type = "response"),
               tolerance = 1e-10)
  expect_equal(predict(reduced, newdata, type = "response"),
               stats::predict(fit, newdata = baked_new)$predicted,
               tolerance = 1e-10)
})

test_that("censored randomForestSRC workflows preserve survival risk", {
  workflow_test_dependencies()
  skip_if_not_installed("censored")
  skip_if_not_installed("randomForestSRC")
  loadNamespace("censored")
  engines <- parsnip::show_engines("rand_forest")
  if (!any(engines$engine == "randomForestSRC" &
           engines$mode == "censored regression")) {
    skip("Installed censored version does not register randomForestSRC")
  }
  data <- workflow_test_survival_data()
  recipe <- recipes::recipe(event ~ age + sex, data = data)
  recipe <- recipes::step_log(recipe, recipes::all_numeric_predictors())
  specification <- parsnip::set_mode(parsnip::rand_forest(trees = 10),
                                     "censored regression")
  specification <- parsnip::set_engine(specification, "randomForestSRC")
  set.seed(211)
  workflow <- workflow_test_fit(recipe, specification, data)
  newdata <- head(data[c("age", "sex")])
  expect_s3_class(burgle(workflow)$model, "burgle_rfsrc")
  workflow_test_compare(workflow, newdata, type = "risk", times = c(100, 500))
})
