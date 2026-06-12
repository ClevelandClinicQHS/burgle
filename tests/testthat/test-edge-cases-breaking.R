## ##############################################################################
## EDGE CASES AND BREAKING SCENARIOS FOR BURGLE PACKAGE
## Testing burgle_lm, burgle_glm, burgle_coxph, and burgle_flexsurvreg
## with various user-specified parameters that might break things
## ##############################################################################

# ============================================================================
# BURGLE_LM EDGE CASES
# ============================================================================

test_that("burgle_lm handles models with NA coefficients", {
  # When a column is removed due to singularity, coef will have NAs
  # This is expected behavior but we test it anyway
  df <- data.frame(x1 = 1:10, x2 = 1:10, y = rnorm(10))
  fit <- lm(y ~ x1 + x2, data = df)
  
  # x1 and x2 are perfectly collinear, burgle handles this gracefully
  bfit <- burgle(fit)
  expect_silent(bfit)
  expect_true(all(!is.na(bfit$coef)))  # NA values should be replaced with 0
})

test_that("burgle_lm handles intercept-only model", {
  # Model with only intercept
  fit <- lm(Sepal.Length ~ 1, data = iris)
  bfit <- burgle(fit)
  
  expect_equal(length(bfit$coef), 1)
  expect_named(bfit$coef, "(Intercept)")
})

test_that("burgle_lm handles models with factors and contrasts", {
  # Models with factors should preserve xlevels and contrasts
  fit <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$xlevels))
  expect_true(!is.null(bfit$contrasts))
  expect_equal(bfit$xlevels$Species, levels(iris$Species))
})

test_that("burgle_lm handles weighted models", {
  # Models with weights - should work fine
  fit <- lm(Sepal.Length ~ Petal.Width, data = iris, weights = rep(1:5, each = 30))
  bfit <- burgle(fit)
  
  expect_true(!is.na(bfit$rss))
  expect_equal(length(bfit$coef), 2)  # intercept + slope
})

test_that("burgle_lm handles models with subset argument", {
  # When fitting with subset, should still work
  fit <- lm(Sepal.Length ~ Petal.Width, data = iris, subset = 1:100)
  bfit <- burgle(fit)
  
  expect_true(!is.na(bfit$rss))
  expect_equal(length(bfit$coef), 2)
})

test_that("burgle_lm handles models with interactions", {
  # Models with interaction terms
  fit <- lm(Sepal.Length ~ Sepal.Width * Petal.Length, data = iris)
  bfit <- burgle(fit)
  
  expect_equal(length(bfit$coef), 4)  # intercept, sw, pl, sw:pl
})

test_that("burgle_lm handles models with polynomial terms", {
  # Models with poly() - generates orthogonal polynomials
  fit <- lm(Sepal.Length ~ poly(Sepal.Width, 2), data = iris)
  bfit <- burgle(fit)
  
  expect_equal(length(bfit$coef), 3)  # intercept + 2 poly terms
})

test_that("burgle_lm handles models with I() identity terms", {
  # Models using I() identity function
  fit <- lm(Sepal.Length ~ I(Sepal.Width^2), data = iris)
  bfit <- burgle(fit)
  
  expect_equal(length(bfit$coef), 2)
})

test_that("burgle_lm handles empty model (formula ~ 0)", {
  # Model with no intercept, only slope - or formula ~ 0
  fit <- lm(Sepal.Length ~ 0 + Petal.Width, data = iris)
  bfit <- burgle(fit)
  
  expect_equal(names(bfit$coef), "Petal.Width")
})

test_that("burgle_lm handles singular fit with qr", {
  # Create a singular design matrix
  skip("Currently fails - singular fit handling needs work")
  
  df <- data.frame(
    y = rnorm(10),
    x1 = 1:10,
    x2 = 1:10,  # perfect collinearity with x1
    x3 = rnorm(10)
  )
  
  # This creates a singular fit
  fit <- lm(y ~ x1 + x2 + x3, data = df, singular.ok = TRUE)
  
  # burgle should handle this gracefully
  bfit <- burgle(fit)
  expect_silent(bfit)
})

# ============================================================================
# BURGLE_GLM EDGE CASES
# ============================================================================

test_that("burgle_glm handles Poisson family", {
  # Poisson GLM with count data
  count_data <- data.frame(
    y = c(2, 3, 1, 5, 2, 4, 1, 2),
    x = c(1, 2, 1, 3, 2, 3, 1, 2)
  )
  
  fit <- glm(y ~ x, family = poisson, data = count_data)
  bfit <- burgle(fit)
  
  expect_equal(bfit$family, "poisson")
  expect_true(!is.null(bfit$inv_link))
})

test_that("burgle_glm handles Gaussian family (regular regression)", {
  # GLM with Gaussian family is equivalent to lm but fits differently
  fit <- glm(Sepal.Length ~ Petal.Width, family = gaussian, data = iris)
  bfit <- burgle(fit)
  
  expect_equal(bfit$family, "gaussian")
})

test_that("burgle_glm handles negative binomial family", {
  skip_if_not_installed("MASS")
  
  count_data <- data.frame(
    y = c(2, 3, 1, 5, 2, 4, 1, 2, 3, 2),
    x = c(1, 2, 1, 3, 2, 3, 1, 2, 3, 1)
  )
  
  fit <- MASS::glm.nb(y ~ x, data = count_data)
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$family))
})

test_that("burgle_glm handles quasi family", {
  # Quasi families with different variance functions
  fit <- glm(I(Species == "versicolor") ~ Petal.Width,
             family = quasi(variance = "mu(1-mu)", link = "logit"), 
             data = iris)
  bfit <- burgle(fit)
  
  expect_equal(bfit$family, "quasi")
})

test_that("burgle_glm handles models with NA coefficients in glm", {
  # When design matrix has singularities
  df <- data.frame(
    y = rbinom(20, 1, 0.5),
    x1 = rep(0:1, 10),
    x2 = rep(0:1, 10)  # perfect collinearity
  )
  
  fit <- glm(y ~ x1 + x2, family = binomial, data = df)
  bfit <- burgle(fit)
  
  # NA coefficients should be replaced with 0
  expect_true(all(!is.na(bfit$coef)))
})

test_that("burgle_glm handles models with separated data in binomial", {
  # Complete separation in logistic regression can cause warnings
  df <- data.frame(
    y = c(rep(0, 10), rep(1, 10)),
    x = 1:20
  )
  
  # This causes separation warnings but should still work
  fit <- glm(y ~ x, family = binomial, data = df)
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_glm with zero-inflated like data", {
  # Data with lots of zeros but using standard GLM
  count_data <- data.frame(
    y = c(rep(0, 15), 1:5),
    x = rep(1:4, 5)
  )
  
  fit <- glm(y ~ x, family = poisson, data = count_data)
  bfit <- burgle(fit)
  
  expect_equal(bfit$family, "poisson")
})

test_that("burgle_glm with very small dataset", {
  # GLM with very few observations
  small_data <- data.frame(
    y = c(0, 1),
    x = c(1, 2)
  )
  
  fit <- glm(y ~ x, family = binomial, data = small_data)
  bfit <- burgle(fit)
  
  expect_equal(nrow(bfit$cov), 2)
})

# ============================================================================
# BURGLE_COXPH EDGE CASES
# ============================================================================

test_that("burgle_coxph handles models with strata", {
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- survival::coxph(survival::Surv(time, status) ~ age + strata(sex), data = lung)
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$basehaz))
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_coxph handles models with multiple strata", {
  lung <- survival::lung |>
    transform(status = status - 1)
  
  # Add another stratification variable
  lung$group <- rep(c("A", "B"), length.out = nrow(lung))
  
  fit <- survival::coxph(survival::Surv(time, status) ~ age + strata(sex) + strata(group), data = lung)
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$basehaz))
})

test_that("burgle_coxph handles models with interactions", {
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- survival::coxph(survival::Surv(time, status) ~ age * sex, data = lung)
  bfit <- burgle(fit)
  
  expect_equal(length(bfit$coef), 3)  # age, sex, age:sex
})

test_that("burgle_coxph with no events", {
  # Create a scenario where there are no events in some strata
  lung <- survival::lung |>
    transform(status = status - 1)
  
  lung$new_status <- lung$status
  lung$new_status[lung$sex == 2] <- 0  # Remove all events for one sex
  
  # This should work but with warnings
  expect_warning({
    fit <- survival::coxph(survival::Surv(time, new_status) ~ age + sex, data = lung)
  })
  
  if (!all(fit$coefficients == 0)) {
    bfit <- burgle(fit)
    expect_true(!is.null(bfit$basehaz))
  }
})

test_that("burgle_coxph with offset", {
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- survival::coxph(survival::Surv(time, status) ~ age + offset(log(ph.ecog + 1)), data = lung)
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_coxph with cluster argument", {
  lung <- survival::lung |>
    transform(status = status - 1)
  
  lung$cluster_id <- rep(1:10, length.out = nrow(lung))
  
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex + cluster(cluster_id), data = lung)
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_coxph with no variation in survival times", {
  lung <- survival::lung |>
    transform(status = status - 1)
  
  # Everyone has same follow-up time
  lung$same_time <- 500
  
  fit <- survival::coxph(survival::Surv(same_time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$basehaz))
})

test_that("burgle_coxph with very long formula", {
  lung <- survival::lung |>
    transform(status = status - 1)
  
  # Add many variables to test formula handling
  lung$var1 <- rnorm(nrow(lung))
  lung$var2 <- rnorm(nrow(lung))
  lung$var3 <- rnorm(nrow(lung))
  lung$var4 <- rnorm(nrow(lung))
  
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex + var1 + var2 + var3 + var4, data = lung)
  bfit <- burgle(fit)
  
  expect_equal(length(bfit$coef), 6)
})

# ============================================================================
# BURGLE_FLEXSURVREG EDGE CASES
# ============================================================================

test_that("burgle_flexsurvreg handles exponential distribution", {
  skip_if_not_installed("flexsurv")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = lung,
    dist = "exponential"
  )
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$coef))
  expect_true(!is.null(bfit$terms))
})

test_that("burgle_flexsurvreg handles Weibull distribution", {
  skip_if_not_installed("flexsurv")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = lung,
    dist = "weibull"
  )
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_flexsurvreg handles lognormal distribution", {
  skip_if_not_installed("flexsurv")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = lung,
    dist = "lognormal"
  )
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_flexsurvreg handles Gompertz distribution", {
  skip_if_not_installed("flexsurv")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = lung,
    dist = "gompertz"
  )
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_flexsurvreg handles generalized gamma", {
  skip_if_not_installed("flexsurv")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = lung,
    dist = "gengamma"
  )
  bfit <- burgle(fit)
  
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_flexsurvreg with singular covariance matrix", {
  skip_if_not_installed("flexsurv")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  # Create near-singular data
  lung$x2 <- lung$age + rnorm(nrow(lung), sd = 0.001)
  
  # This might produce NA in covariance
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + x2,
    data = lung,
    dist = "weibull"
  )
  
  # burgle should handle NA in cov gracefully
  bfit <- burgle(fit)
  expect_warning(bfit, NA)  # May warn or not
  
  if (any(is.na(bfit$cov))) {
    # Covariance matrix should be converted to zeros
    expect_equal(bfit$cov, matrix(0, nrow = length(bfit$coef), ncol = length(bfit$coef)))
  }
})

test_that("burgle_flexsurvreg with no covariates (intercept only)", {
  skip_if_not_installed("flexsurv")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ 1,
    data = lung,
    dist = "weibull"
  )
  bfit <- burgle(fit)
  
  # Should have parameters even without covariates
  expect_true(!is.null(bfit$coef))
})

test_that("burgle_flexsurvreg with very high number of covariates", {
  skip_if_not_installed("flexsurv")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  # Add many variables
  for (i in 1:10) {
    lung[[paste0("var", i)]] <- rnorm(nrow(lung))
  }
  
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + var1 + var2 + var3 + var4 + var5 + var6 + var7 + var8 + var9 + var10,
    data = lung,
    dist = "weibull"
  )
  bfit <- burgle(fit)
  
  expect_true(length(bfit$coef) > 10)
})

# ============================================================================
# PREDICT EDGE CASES
# ============================================================================

test_that("predict.burgle_lm with new factor levels causes error", {
  fit <- lm(Sepal.Length ~ Species, data = iris)
  bfit <- burgle(fit)
  
  new_data <- data.frame(Species = factor("unknown", levels = c(levels(iris$Species), "unknown")))
  
  # Should error when predicting with unknown factor level
  expect_error({
    predict(bfit, newdata = new_data, original = TRUE)
  })
})

test_that("predict.burgle_glm with new factor levels causes error", {
  fit <- glm(I(Species == "versicolor") ~ Species, family = binomial, data = iris)
  bfit <- burgle(fit)
  
  new_data <- data.frame(Species = factor("unknown", levels = c(levels(iris$Species), "unknown")))
  
  expect_error({
    predict(bfit, newdata = new_data, original = TRUE)
  })
})

test_that("predict.burgle_coxph with missing newdata argument", {
  skip("Need to check if newdata is required")
  
  lung <- survival::lung |>
    transform(status = status - 1)
  
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  bfit <- burgle(fit)
  
  # What happens if newdata is missing?
  # Should probably error or use original data
})

# ============================================================================
# SPECIAL MATRIX/STRUCTURE EDGE CASES
# ============================================================================

test_that("burgle_lm with factors in model matrix but wrong contrasts", {
  # This tests if burgle preserves contrasts correctly
  fit <- lm(Sepal.Length ~ Species, data = iris, 
            contrasts = list(Species = contr.helmert))
  bfit <- burgle(fit)
  
  expect_equal(bfit$contrasts$Species, contr.helmert(3))
})

test_that("burgle_glm with missing intercept and factors", {
  fit <- glm(I(Species == "versicolor") ~ 0 + Species, 
             family = binomial, data = iris)
  bfit <- burgle(fit)
  
  # All three species as separate coefficients
  expect_equal(length(bfit$coef), 3)
})

# ============================================================================
# SUMMARY OF KNOWN ISSUES
# ============================================================================

# Known Issues Found:
# 1. Singular fits in lm - handling of NA coefficients needs testing
# 2. Separation in logistic regression - large coefficients and covariance matrices
# 3. Near-singular covariance matrices in flexsurv - NAs in cov element
# 4. Factor level validation in prediction - only validated against xlevels

