# Burgle Package: Edge Cases and Breaking Scenarios Analysis

## Overview
This document identifies potential edge cases and failure modes in the burgle package when users specify various parameters in `lm()`, `glm()`, `coxph()`, and `flexsurvreg()` model objects.

---

## BURGLE_LM Edge Cases and Issues

### 1. **Singular Fits with NA Coefficients**
**When it breaks:** When fitting a model with perfectly collinear columns
```r
df <- data.frame(x1 = 1:10, x2 = 1:10, y = rnorm(10))
fit <- lm(y ~ x1 + x2, data = df)  # x1 and x2 are collinear
```
**Error/Warning:** 
- `lm()` returns NAs in coefficients
- `vcov()` returns NAs in covariance matrix
- Line 31 in burgle.R: `coef[is.na(coef)] <- 0` handles NAs but silently replaces them

**Current Code Behavior:**
```r
# In burgle.lm (R/burgle.R, line 29-32):
if(any(is.na(coef))){
  warning("At least 1 coefficient has a vlue of NA")  # Note: typo "vlue"
  coef[is.na(coef)] <- 0
  cov[is.na(cov)] <- 0
}
```
**Solution Status:** ✓ Already handled with replacement
**Possible Enhancement:** Store a flag indicating which coefficients were NA-replaced for prediction warnings

---

### 2. **Intercept-Only Models**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ 1, data = iris)
bfit <- burgle(fit)
```
**Issue:** Single coefficient model - edge case for prediction matrix operations

**Solution Status:** ✓ Should work fine
**Testing:** Verify predict doesn't break with 1-dimensional coef

---

### 3. **Models with Factors and Contrasts**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
bfit <- burgle(fit)
```
**Issue:** Factor levels preserved in `xlevels`, contrasts preserved in `contrasts`
- However, if predicting with new/unknown factor levels → breaks prediction

**Current Code:** Stores `object$xlevels` and `object$contrasts` ✓

**Solution Status:** ✓ Partially - needs validation in predict

**Recommendation:** Add validation in `predict.burgle_lm` to check factor levels in newdata

---

### 4. **Weighted Models**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ Petal.Width, weights = rep(1:5, each = 30), data = iris)
bfit <- burgle(fit)
```
**Issue:** Line 10 in burgle_lm.R calculates RSS (Residual Sum of Squares) as:
```r
rss <- sum(object$residuals ^2)/object$df.residual
```
- This is correct for weighted models (R handles it internally)
- Weights are not stored but not needed for predictions
- Note: The function returns `rss`, not `mse`

**Solution Status:** ✓ Works correctly

---

### 5. **Models with subset Argument**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ Petal.Width, subset = 1:100, data = iris)
bfit <- burgle(fit)
```
**Issue:** df.residual is correct for subset, residuals only for subset
**Solution Status:** ✓ Works correctly

---

### 6. **Models with Interactions**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ Sepal.Width * Petal.Length, data = iris)
bfit <- burgle(fit)
```
**Issue:** Interaction terms are in the formula terms object
**Solution Status:** ✓ Works - terms object captures interaction

---

### 7. **Models with Polynomial Terms using poly()**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ poly(Sepal.Width, 2), data = iris)
bfit <- burgle(fit)
```
**Issue:** `poly()` creates orthogonal polynomials with attributes
- These attributes are lost in prediction if not re-applied
- Line 40 in burgle.R: `terms <- stats::delete.response(terms)` preserves poly() call

**Solution Status:** ✓ Should work because poly() info in terms

**Testing Needed:** Verify predictions match original model

---

### 8. **Models using I() Identity Function**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ I(Sepal.Width^2), data = iris)
```
**Issue:** I() prevents formula interpretation, terms object should capture it
**Solution Status:** ✓ Should work

---

### 9. **Models with No Intercept (~ 0 or ~ -1)**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ 0 + Petal.Width, data = iris)
```
**Issue:** Affects model matrix dimensions and coefficient interpretation
- Important: MSE calculation still valid
**Solution Status:** ✓ Works - terms object handles this

---

### 10. **Empty Data**
**When it breaks:**
```r
df <- data.frame(x = numeric(0), y = numeric(0))
fit <- lm(y ~ x, data = df)
```
**Issue:** df.residual = -1, causes division by negative
**Solution Status:** ✗ **POTENTIAL BUG** - Check if lm() even allows empty data

---

## BURGLE_GLM Edge Cases and Issues

### 1. **Different Family Functions**
**Currently Tested:**
- ✓ `family = binomial(link = "logit")` 
- ✓ `family = gaussian()`
- ✓ `family = poisson()`

**Potentially Problematic:**
- `family = binomial(link = "identity")` - Can produce predictions outside [0,1]
- `family = binomial(link = "cloglog")` 
- `family = quasibinomial()`
- `family = quasi(variance = "mu(1-mu)", link = "logit")`

**Issue:** Line 82-84 in burgle.R:
```r
family <- object$family$family
inv_link <- object$family$linkinv
```
- This should work for all families
- BUT: `quasi` family might have issues

**Solution Status:** ✗ **Needs Testing** - quasi family might break

**Recommendation:** Test all standard R family functions

---

### 2. **NA Coefficients in GLM**
**When it breaks:**
```r
df <- data.frame(y = rbinom(20, 1, 0.5), x1 = rep(0:1, 10), x2 = rep(0:1, 10))
fit <- glm(y ~ x1 + x2, family = binomial, data = df)
```
**Current Handling:** Lines 66-70 in burgle.R replace NAs with 0
**Solution Status:** ✓ Already handled

---

### 3. **Complete/Quasi-Complete Separation in Logistic Regression**
**When it breaks:**
```r
df <- data.frame(y = c(rep(0, 10), rep(1, 10)), x = 1:20)
fit <- glm(y ~ x, family = binomial, data = df)
```
**Warning:** "fitted probabilities numerically 0 or 1 occurred"
**Issue:** 
- Produces very large coefficients
- Covariance matrix may be singular or unstable
- MSE calculation (line 72) still valid

**Solution Status:** ✓ Works but may produce extreme predictions

**Recommendation:** Warn user if coefficients are very large (|coef| > 10)

---

### 4. **Zero-Inflated or High-Censoring Data with Standard GLM**
**When it breaks:**
```r
count_data <- data.frame(y = c(rep(0, 15), 1:5), x = rep(1:4, 5))
fit <- glm(y ~ x, family = poisson, data = count_data)
```
**Issue:** Model assumes simple Poisson, but data is zero-inflated
- This is a model misspecification, not a burgle bug
- burgle works correctly with the wrong model

**Solution Status:** ✓ Not a burgle issue - user responsibility

---

### 5. **Very Small Dataset**
**When it breaks:**
```r
small_data <- data.frame(y = c(0, 1), x = c(1, 2))
fit <- glm(y ~ x, family = binomial, data = small_data)
```
**Issue:** Perfect separation with only 2 obs, coef and cov may be Inf/NaN
**Solution Status:** ? Needs testing

---

### 6. **MASS::glm.nb (Negative Binomial)**
**When it breaks:**
```r
fit <- MASS::glm.nb(y ~ x, data = count_data)
```
**Issue:** Is `glm.nb` a standard glm object?
- Need to check if burgle.glm method is called
- May need separate burgle.negbin method

**Solution Status:** ✗ **Needs Testing** - class dispatch issue

---

## BURGLE_COXPH Edge Cases and Issues

### 1. **Models with Strata**
**When it breaks:**
```r
fit <- coxph(Surv(time, status) ~ age + strata(sex), data = lung)
```
**Current Code (Line 11-17 in burgle_cph.R):**
```r
if (!is.null(object$xlevels) && (!is.null(object$strata) |
                                any(grepl("strata", names(object$xlevels))))) {
  bh0 <- bh[, c("hazard", "strata")]
  bh <- bh[!duplicated(bh0), ]
  terms <- drop.special(terms, attr(terms, "specials")$strata)
}
```
**Issue:** 
- Handles strata but removes duplicates in basehaz
- This is complex code with potential edge cases

**Solution Status:** ✓ Appears to work but needs validation

**Testing:** Compare basehaz between different strata

---

### 2. **Models with Multiple Strata**
**When it breaks:**
```r
fit <- coxph(Surv(time, status) ~ age + strata(sex) + strata(group), data = lung)
```
**Issue:** Multiple strata in basehaz creates interaction structure
- Complex duplicated() logic may not handle correctly

**Solution Status:** ? Needs testing

**Recommendation:** Test with 2+ strata variables

---

### 3. **Models with Interactions**
**When it breaks:**
```r
fit <- coxph(Surv(time, status) ~ age * sex, data = lung)
```
**Issue:** Interaction terms in coefficients and terms object
**Solution Status:** ✓ Should work - terms handles interactions

---

### 4. **No Events in Some Strata**
**When it breaks:**
```r
lung$new_status <- lung$status
lung$new_status[lung$sex == 2] <- 0
fit <- coxph(Surv(time, new_status) ~ age + sex, data = lung)
```
**Warning:** "X>=0 is violated by a standard deviation that is close to 0"
**Issue:** 
- Basehaz calculation may fail
- Coefficients may be Inf or NaN
- basehaz() may return empty

**Solution Status:** ✗ **POTENTIAL BUG** - Line 5 in burgle_cph.R:
```r
bh <- suppressWarnings(survival::basehaz(object, centered = FALSE))
```
If basehaz is empty or NA, subsequent lines fail

---

### 5. **Models with Offset**
**When it breaks:**
```r
fit <- coxph(Surv(time, status) ~ age + offset(log(ph.ecog + 1)), data = lung)
```
**Issue:** Offset appears in terms but not in coef (no coefficient for offset)
- Line 21: `coef <- stats::coef(object)` - no offset term
- This is correct behavior

**Solution Status:** ✓ Works correctly

---

### 6. **Models with cluster() Argument**
**When it breaks:**
```r
fit <- coxph(Surv(time, status) ~ age + cluster(cluster_id), data = lung)
```
**Issue:** cluster() affects SE but not coefficients
- Similar to offset, cluster is handled by coxph internally
- Line 21: coef doesn't include cluster term

**Solution Status:** ✓ Works - cluster doesn't affect coef/cov extraction

---

### 7. **Time-Varying Covariates**
**When it breaks:**
```r
# Incorrect use with Surv(time, time2, status)
fit <- coxph(Surv(time, time2, status) ~ age + ..., data = lung)
```
**Issue:** 
- burgle doesn't handle time-varying covariates specifically
- basehaz() works fine but predictions would be wrong
- Line 5: basehaz(object, centered=FALSE) treats as simple coxph

**Solution Status:** ✗ **NOT SUPPORTED** - Should error or warn

**Recommendation:** Add check for time-varying covariates and error

---

### 8. **No Variation in Survival Times**
**When it breaks:**
```r
lung$same_time <- 500
fit <- coxph(Surv(same_time, status) ~ age + sex, data = lung)
```
**Issue:** All survival times identical
- basehaz() may have only 1 row
- Line 14/19: duplicated() logic still works

**Solution Status:** ? Needs testing

---

### 9. **Very Long Formulas**
**When it breaks:**
```r
fit <- coxph(Surv(time, status) ~ age + sex + var1 + var2 + ... + var20, data = lung)
```
**Issue:** Not a burgle-specific issue
**Solution Status:** ✓ Works fine

---

## BURGLE_FLEXSURVREG Edge Cases and Issues

### 1. **Different Distributions**
**Supported in flexsurv:**
- exponential, weibull, gamma, lognormal, gompertz, loglogistic
- gengamma (generalized gamma), genf (generalized F)
- etc.

**Current Code (burgle_flexsurv.R):**
```r
pf <- object$dfns$p
hz <- object$dfns$H
qn <- object$dfns$q
```

**Issue:** 
- These are functions from flexsurv distribution object
- Should work for all distributions
- BUT: Line 7 assumes `object$covdata$terms` exists

**Solution Status:** ? Needs testing with all distributions

---

### 2. **NA/Singular Covariance Matrix**
**When it breaks:**
```r
lung$x2 <- lung$age + rnorm(nrow(lung), sd = 0.001)  # Nearly collinear
fit <- flexsurvreg(Surv(time, status) ~ age + x2, dist = "weibull", data = lung)
```
**Current Code (Line 14-18 in burgle_flexsurv.R):**
```r
if(any(is.na(cov))){
  warning("No covariance estimates found, predicting will only be done from the estimated model")
  cov <- matrix(0, nrow = length(coef), ncol = length(coef))
}
```
**Issue:**
- Replaces NA with 0 matrix (no uncertainty)
- Warning message is clear
- Users won't get parameter uncertainty in predictions

**Solution Status:** ✓ Handled but limited

**Recommendation:** This is correct behavior - 0 cov means no uncertainty

---

### 3. **No Covariates (Intercept Only)**
**When it breaks:**
```r
fit <- flexsurvreg(Surv(time, status) ~ 1, dist = "weibull", data = lung)
```
**Issue:**
- Only distribution parameters (scale, shape for Weibull)
- No regression coefficients for covariates
- Line 6: `coef <- stats::coef(object)` still works
- Line 11: empty coef matrix = 0x0

**Solution Status:** ? Needs testing

**Potential Issue:** Line 51 in burgle_flexsurv.R:
```r
nc <- names(object$coef)
```
If no covariates, names might be dist parameters only

---

### 4. **Complex Distribution Specifications**
**When it breaks:**
```r
# Custom distribution via dfns
fit <- flexsurvreg(..., dist = "custom_dist", dfns = list(...))
```
**Issue:** burgle assumes standard flexsurv distributions
**Solution Status:** ✗ **Likely Broken** with custom distributions

**Recommendation:** Add check for standard dists, error on custom

---

### 5. **Very High Number of Covariates**
**When it breaks:**
```r
fit <- flexsurvreg(Surv(time, status) ~ age + var1 + ... + var50, dist = "weibull", data = lung)
```
**Issue:** Not flexsurv-specific
**Solution Status:** ✓ Works fine

---

### 6. **Interactions and Complex Formulas**
**When it breaks:**
```r
fit <- flexsurvreg(Surv(time, status) ~ age * sex, dist = "weibull", data = lung)
```
**Issue:** Interactions in covdata$terms
**Solution Status:** ? Needs testing

---

## PREDICTION Edge Cases

### 1. **Unknown Factor Levels**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ Species, data = iris)
bfit <- burgle(fit)
new_data <- data.frame(Species = factor("unknown", levels = c(levels(iris$Species), "unknown")))
predict(bfit, newdata = new_data)
```
**Current Code:** Lines in predict_burgle.R call `model.matrix()` which checks xlev
**Solution Status:** ✓ Should error appropriately

**Testing Needed:** Verify error message

---

### 2. **Missing Required Variables**
**When it breaks:**
```r
fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
bfit <- burgle(fit)
predict(bfit, newdata = data.frame(Sepal.Width = 1))  # Missing Petal.Length
```
**Current Code:** `model.matrix()` will error with missing variable
**Solution Status:** ✓ Should error

---

### 3. **NULL or NA Newdata**
**When it breaks:**
```r
predict(bfit, newdata = NULL)
predict(bfit, newdata = NA)
```
**Issue:** Different functions handle this differently
**Solution Status:** ? Needs checking per function

---

## Summary of Severity Levels

### 🔴 Critical Issues (Likely to Break)
1. **Singular covariance in flexsurv** - Produces 0 matrix (acceptable but needs docs)
2. **No events in coxph strata** - basehaz() may fail
3. **Time-varying covariates in coxph** - Not supported, should error
4. **Custom distributions in flexsurv** - Likely to break

### 🟡 Moderate Issues (Edge Cases)
1. **Complete separation in GLM** - Large coefficients, valid but extreme
2. **Empty design matrices** - Division by zero possibilities
3. **Multiple strata in coxph** - Complex deduplication logic
4. **quasi() family in GLM** - Untested, may have issues

### 🟢 Minor Issues (Handled Well)
1. NA coefficients - Already replaced with 0
2. Factor contrasts - Preserved and handled
3. Interactions/polynomials - Handled via terms object
4. Weighted/subset models - Handled correctly

---

## Recommendations for Package Improvement

1. **Add validation function** to check for problematic model specifications
2. **Add warnings** for singular fits, separation, etc.
3. **Document limitations** for strata, interactions, etc.
4. **Test matrix** covering all cases in test-edge-cases-breaking.R
5. **Error handling** for unsupported scenarios (time-varying, custom dist)
6. **Fix typo** in warning message: "vlue" → "value"

---

## Test File Location
See: `tests/testthat/test-edge-cases-breaking.R` for executable tests covering these cases
