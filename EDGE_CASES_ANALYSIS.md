# Burgle Package: Edge Cases and Breaking Scenarios Analysis

**Last Updated:** 2026-07-06  
**Purpose:** Systematic identification and documentation of edge cases and failure modes in the burgle package for model uncertainty quantification.

## Overview
This document identifies potential edge cases and failure modes in the burgle package when users specify various parameters in `lm()`, `glm()`, `coxph()`, and `flexsurvreg()` model objects.

---

## Table of Contents

1. [Quick Reference](#quick-reference)
2. [LM Models](#burgle_lm-edge-cases-and-issues)
3. [GLM Models](#burgle_glm-edge-cases-and-issues)
4. [Cox Proportional Hazards](#burgle_coxph-edge-cases-and-issues)
5. [Flexible Survival Regression](#burgle_flexsurvreg-edge-cases-and-issues)
6. [Prediction Edge Cases](#prediction-edge-cases)
7. [Severity Levels Summary](#summary-of-severity-levels)
8. [Recommendations](#recommendations-for-package-improvement)

---

## Quick Reference

| Model Type | Critical Issues | Moderate Issues | Resolution |
|-----------|-----------------|-----------------|-----------|
| **LM** | Empty data, singular fits | Contrasts validation | Handle in draw_models() |
| **GLM** | quasi() family untested | Complete separation | Test all families |
| **COXPH** | No events in strata, time-varying | Multiple strata | Error or warn users |
| **FLEXSURV** | Custom distributions | Singular covariance | Use 0 cov matrix |

---

## BURGLE_LM Edge Cases and Issues

**Implementation File:** `R/burgle_lm.R`  
**Prediction File:** `R/predict_burgle.R`

### 1. **Singular Fits with NA Coefficients**

**When it breaks:** When fitting a model with perfectly collinear columns

```r
df <- data.frame(x1 = 1:10, x2 = 1:10, y = rnorm(10))
fit <- lm(y ~ x1 + x2, data = df)  # x1 and x2 are collinear
bfit <- burgle(fit)
```

**Expected Behavior:**
- `lm()` returns NAs in coefficients due to collinearity
- `vcov()` returns NAs in covariance matrix

**Current Code Behavior:** ✓ **Handled**
- `burgle()` preserves the original model including NA coefficients
- NA replacement with 0 occurs in `draw_models()` during prediction stage
- Warning issued when predictions are made with NA coefficients replaced

**Implementation Details:**
In `R/predict_burgle.R`, the `draw_models()` function handles NA coefficients:
```r
# NA Coefficient Handling in draw_models()
if(!is.null(dim(models))){
  na_mask <- is.na(models)
  if(any(na_mask)){
    # Replace NAs with 0 during prediction
    models[na_mask] <- 0
  }
}
```

**Solution Status:** ✓ Handled with replacement during prediction stage  
**Design Rationale:** NA replacement is kept in the predict stage to preserve the original model object integrity

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
**Current Handling:** NA coefficients are preserved in the burgle object and replaced with 0 during the prediction stage in `draw_models()` function

**Solution Status:** ✓ Handled during prediction stage

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

**Implementation File:** `R/burgle_cph.R`  
**Prediction File:** `R/predict_burgle.R` (as `predict.burgle_coxph`)

### 1. **Models with Strata**

**When it breaks:**
```r
fit <- coxph(Surv(time, status) ~ age + strata(sex), data = lung)
bfit <- burgle(fit)
```

**Why Strata Matter:**
- Strata create separate baseline hazards for each stratum
- Must be handled separately in predictions
- `basehaz()` returns combined hazard for all strata

**Current Implementation** (`R/burgle_cph.R`, lines 11-17):
```r
# Strata Handling in burgle.cph
has_strata <- !is.null(object$strata) || "strata" %in% colnames(bh)
if (has_strata) {
  # Remove duplicate hazard entries across strata
  bh0 <- bh[, c("hazard", "strata")]
  bh <- bh[!duplicated(bh0), ]
  # Strip strata from terms for later model matrix construction
  terms <- strip_strata_terms(terms)
} else {
  # Non-stratified: remove only duplicate hazard values
  bh <- bh[!duplicated(bh$hazard), ]
}
```

**Key Logic:**
- Detects stratification by checking `object$strata` attribute
- Removes duplicate combinations of (hazard, strata) pairs
- Calls `strip_strata_terms()` helper to clean formula for predictions

**Solution Status:** ✓ Appears to work but needs validation

**Testing Recommendation:** Verify baseline hazard values match original model across strata

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

**Implementation File:** `R/burgle_flexsurv.R`  
**Key Function:** `burgle.flexsurvreg()` and `predict.burgle_flexsurvreg()`

### 1. **Different Distributions**

**Supported Distributions:**
- **Parametric:** exponential, weibull, gamma, lognormal, gompertz, loglogistic
- **Advanced:** gengamma (generalized gamma), genf (generalized F)
- And others supported by flexsurv package

**Current Implementation** (`R/burgle_flexsurv.R`, lines 21-36):
```r
# Distribution-specific functions extracted from flexsurv object
pf <- object$dfns$p              # CDF function
hz <- object$dfns$H              # Cumulative hazard function
qn <- object$dfns$q              # Quantile function
inv_t <- object$dlist$inv.transforms  # Parameter transforms
pars_i <- object$basepars        # Base parameter indices
loc <- which(names(coef) == object$dlist$location)  # Location parameter index
```

**Key Design:**
- Extracts distribution functions from `object$dfns` (works universally)
- Parameters stored in flexible way for different distributions
- Base parameters (shape, scale) distinguished from location parameters

**Solution Status:** ✓ Design is general, but needs testing with all distributions

---

### 2. **NA/Singular Covariance Matrix**

**When it happens:**
```r
# Nearly collinear covariates in survival model
lung$x2 <- lung$age + rnorm(nrow(lung), sd = 0.001)
fit <- flexsurvreg(Surv(time, status) ~ age + x2, dist = "weibull", data = lung)
bfit <- burgle(fit)
```

**Handling Strategy** (`R/burgle_flexsurv.R`, lines 15-18):
```r
# Singular Covariance Handling
if(any(is.na(cov))){
  warning("No covariance estimates found, predicting will only be done from the estimated model")
  cov <- matrix(0, nrow = length(coef), ncol = length(coef))
}
```

**Implications:**
- Zero covariance matrix = no uncertainty in predictions
- Users only get point estimates, not prediction intervals
- Clear warning message explains limitation

**Solution Status:** ✓ Handled appropriately  
**Design Rationale:** This is correct behavior—preserves functionality while limiting false confidence

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

### 🔴 Critical Issues (Likely to Break - Action Required)

| Issue | Model | Impact | Resolution |
|-------|-------|--------|-----------|
| No events in strata | COXPH | `basehaz()` may fail | Add validation check |
| Time-varying covariates | COXPH | Unsupported use case | Return error message |
| Custom distributions | FLEXSURV | Likely incompatible | Add dist validation |
| Empty data | LM/GLM | Division by zero | Input validation |

---

### 🟡 Moderate Issues (Edge Cases - Testing Needed)

| Issue | Model | Risk Level | Workaround |
|-------|-------|-----------|-----------|
| Complete separation | GLM | Large coefficients | Document limitation |
| Multiple strata | COXPH | Complex deduplication | Needs testing |
| quasi() family | GLM | Untested behavior | Add family tests |
| Intercept-only models | FLEXSURV | Empty parameter names | Boundary testing |

---

### 🟢 Minor Issues (Handled Well - Low Priority)

✓ **NA coefficients** - Already replaced with 0 during prediction  
✓ **Factor contrasts** - Preserved and correctly stored  
✓ **Interactions/polynomials** - Handled via terms object  
✓ **Weighted/subset models** - Handled correctly  
✓ **Offset terms** - Properly excluded from coefficient extraction

---

## Recommendations for Package Improvement

### Priority 1: Critical Validation (Must Have)

1. **Add Input Validation Function** (`validate_model()`)
   - Check for time-varying covariates in Cox models
   - Verify no custom flexsurv distributions
   - Detect empty datasets
   - **File to create:** `R/validate_burgle_inputs.R`

2. **Fix COXPH Edge Case** - Empty strata events
   - Add `try()` wrapper around `basehaz()` call
   - Return informative error if baseline hazard computation fails
   - **File:** `R/burgle_cph.R`, line 5

3. **Add Factor Level Validation**
   - Check newdata factor levels during prediction
   - Provide clear error message for unknown levels
   - **File:** `R/predict_burgle.R`

---

### Priority 2: Testing & Documentation (Should Have)

4. **Expand Test Coverage**
   - Create matrix covering all family functions for GLM
   - Test all flexsurv distributions
   - Test multiple strata combinations
   - **File:** `tests/testthat/test-edge-cases-breaking.R`

5. **Add Inline Documentation**
   - Document MSE calculation logic in `burgle_lm.R`
   - Add strata handling explanation in `burgle_cph.R`
   - Document parameter extraction in `burgle_flexsurv.R`

6. **User-Facing Documentation**
   - Create vignette: "Known Limitations"
   - Add FAQ section to README
   - Document quasi() family status

---

### Priority 3: Code Quality (Nice to Have)

7. **Code Cleanup**
   - Remove commented-out code in `burgle_flexsurv.R` (lines 103-106, 153-167)
   - Simplify factor level checking logic
   - Create helper function for duplicated() deduplication logic

8. **Standardize Error Messages**
   - Consistent format across all functions
   - Include suggested workarounds when possible
   - **Current typo:** "vlue" → "value" in warning message

9. **Performance Optimization**
   - Profile strata deduplication logic for large datasets
   - Consider vectorization for matrix operations

---

### Implementation Timeline

| Phase | Tasks | Est. Effort |
|-------|-------|------------|
| **Phase 1** | Validation, basehaz fix, tests | 2-3 days |
| **Phase 2** | Documentation, inline comments | 1 day |
| **Phase 3** | Code cleanup, refactoring | 1 day |

---

## Test File Location
See: `tests/testthat/test-edge-cases-breaking.R` for executable tests covering these cases
