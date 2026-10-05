
<!-- README.md is generated from README.Rmd. Please edit that file -->

# burgle <img src="man/figures/logo.png" align="right" height="139" alt="" />

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/burgle)](https://cran.r-project.org/package=burgle)
![CRAN_Download_Counter](http://cranlogs.r-pkg.org/badges/grand-total/burgle)

The goal of burgle is to “steal” only the necessary parts of model
objects for applications in simulation.

## Installation

``` r
install.packages("burgle")
```

Or you can install the development version of burgle like so:

``` r
devtools::install_github("ClevelandClinicQHS/burgle")
```

## Pros of `burgle`

1.  The reduction in size can save on memory and storage space. A great
    advantage since R works with physical memory
2.  The removal of data from the model objects can allow them to be
    shared freely, if there are data sharing concerns or requirements.
3.  A streamlined method to simulate response for parameter uncertainty
    and probabilistic sampling

## Fitted workflows

`burgle()` accepts fitted tidymodels workflows with a trained recipe and a
data-frame/tibble blueprint. It compiles preprocessing into ordered operations
containing trained parameters, then calls the fitted engine's existing
`burgle()` method. Prediction transforms new predictors before dispatching to
that burgled engine. The returned object retains neither a workflow nor a recipe
and never calls `recipes::bake()` during prediction.

### Supported-model inventory

| Engine class | Retained prediction metadata | Prediction contract / tidymodels integration |
|:--|:--|:--|
| `lm` | coefficients, covariance, residual variance, terms, levels, contrasts | parsnip `linear_reg(engine = "lm")`; `lp`, `link`, `response`, draws, simulations, `se`, `se_type`, limits, seed |
| `glm` | coefficients, covariance, residual variance, terms, levels, contrasts, family, inverse link | parsnip `logistic_reg(engine = "glm")`; all existing GLM families via the engine method; `lp`, `link`, binomial `response`, draws, simulations, `se`, seed |
| `multinom` | flattened class coefficients, covariance, reference/response levels, terms, contrasts | parsnip `multinom_reg(engine = "nnet")`; `lp`, `odds`, sampled `response`, `floor`, draws, simulations, seed |
| `coxph` | coefficients, covariance, terms, levels, contrasts, baseline hazards | censored `proportional_hazards(engine = "survival")`; `lp`, risk/response at times, draws, simulations, seed, `predict_time()` |
| `cph` | coefficients, covariance, terms, levels, contrasts, baseline hazards | no built-in parsnip/censored adapter; existing Cox prediction contract |
| `flexsurvreg` | distribution parameters and indices, covariance, terms, levels, contrasts, distribution functions/transforms, event times | censored `survival_reg(engine = "flexsurv")`; existing `lp`, risk/response/time, draws, simulations, seed, `predict_time()` |
| `CauseSpecificCox` | per-cause coefficients/covariances, terms, levels, contrasts, hazards and event times | no built-in adapter; `lp`, risk/response, cause, times, draws, simulations, `predict_time()` |
| `rfsrc` | burgled native forest and its prediction metadata, not GLM coefficients | no built-in parsnip/censored adapter; regression/classification/survival/competing-risk modes, risk/response, simulations, cause, times and engine arguments |

An adapter must produce one of these fitted classes; no unsupported engine is
made supported by wrapping it in a workflow. Engines without a built-in adapter
are tested by composing their actual burgled models with compiled preprocessing.
There is no XGBoost method in this version of burgle.

The wrapper does **not** translate prediction argument meanings or return
shapes into parsnip conventions. In particular, burgle GLM `type = "link"`
applies the inverse link (probabilities for binomial fits), while `response`
samples outcomes. Uncertainty, seeds and limitations are exactly those of the
existing engine method. Fitted terms, contrasts, coefficient order and every
model-specific covariance/parameter structure remain unchanged: transformations
reproduce the trained predictor columns rather than replacing the engine's
design matrix with a newly inferred one.

### Step and runtime-dependency inventory

| Steps | Compiled information | Prediction dependency |
|:--|:--|:--|
| `step_poly()` | raw/orthogonal degree, learned orthogonal coefficients, output dimensions/names | stats |
| `step_ns()`, `step_bs()` | knots, boundary knots, intercept, degree, dimensions/names | splines |
| `step_spline_b()`, `step_spline_natural()` | trained splines2 basis attributes/options and dimensions/names | splines2 |
| `step_poly_bernstein()` | trained Bernstein basis attributes and dimensions/names | splines2 |
| `step_spline_monotone()`, `step_spline_convex()`, `step_spline_nonnegative()` | trained I-, C-, and M-spline attributes/options and dimensions/names | splines2 |
| `step_interact()` | resolved interaction terms, factor expansion, contrasts, output names/separator | stats |
| `step_harmonic()` | frequencies, starting values, cycle size, sin/cos ordering | base R |
| `step_log()`, `step_sqrt()`, `step_inverse()`, `step_invlogit()`, `step_logit()` | selected columns, configured bases/offsets/signed behavior where available | base R / stats |
| `step_abs()` | selected columns; absolute values | base R |
| `step_ratio()` | trained numerator/denominator pairs and resolved output names | base R |
| `step_lag()` | lags, fill value, prefix, selected columns, generated order | dplyr |

The recipe compiler follows the actual recipes 1.0.9 implementations. All
listed steps except `step_abs()` are supplied by recipes; `burgle::step_abs()`
is a recipes extension supplied here because recipes does not provide that
step. It uses base `abs()` semantics. Workflow extraction requires workflows
and recipes; prediction only needs the dependencies in the table, plus those
required by the underlying burgled model. Install optional packages separately.
Prepared basis matrices, recipe templates, training observations and selection
closures are not retained. `saveRDS()` / `readRDS()` preserves compiled objects.


```r
library(workflows)
library(parsnip)
library(recipes)

rec <- recipe(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris) |>
  step_log(Petal.Length, offset = 1) |>
  step_ns(Sepal.Width, deg_free = 3) |>
  step_interact(terms = ~ starts_with("Sepal.Width_ns_"):Petal.Length)
wf <- workflow() |>
  add_recipe(rec) |>
  add_model(linear_reg() |> set_engine("lm")) |>
  fit(iris)
compact <- burgle(wf)
predict(compact, head(iris), type = "lp")
predict(compact, head(iris), original = FALSE, draws = 3, seed = 42)
```

### Omitted preprocessing and limitations

Unsupported steps warn with their identity and position and are omitted, not
silently treated as supported. Inference-time `skip = TRUE` steps are not
evaluated. Missing dependencies produce actionable errors: supply required
omitted preprocessing outputs externally. A skipped transformation of an
existing column may be structurally valid but **still changes the meaning of
the input**; there is no prediction-equivalence guarantee for such workflows.
Fitted model terms and parameters are never dropped to accommodate omissions.


```r
centered <- workflow() |>
  add_recipe(recipe(Sepal.Length ~ Sepal.Width, data = iris) |>
               step_center(Sepal.Width)) |>
  add_model(linear_reg() |> set_engine("lm")) |>
  fit(iris)
compact <- burgle(centered) ## Warns: step_center is omitted.
external <- head(iris)
external$Sepal.Width <- external$Sepal.Width - mean(iris$Sepal.Width)
predict(compact, external) ## Supply the omitted centering yourself.
```

Recipe execution order, trained selections, generated names and
`keep_original_cols` behavior are preserved. Lags use each batch's supplied row
order and fill leading rows independently: no sorting, grouping, lookback or
cross-call state is added. Factor and missing/domain-value behavior follows the
compiled step and then the underlying model; models that omit missing rows may
therefore return fewer predictions. Rank-deficient/aliased fits retain their
existing engine limitations, including coefficient warnings and uncertainty
limitations. Formula/variable workflow preprocessors, matrix blueprints and
workflow postprocessors are explicitly rejected; use a zero-step recipe for
ordinary predictors. Empty and single-row batches remain subject to the
underlying model's supported prediction contract.
## Linear Model Example

``` r
set.seed(287453)
library(burgle)
fit <- lm(Sepal.Length ~., data = iris)
bfit <- burgle(fit)
pryr::object_size(fit)
#> 39.43 kB
pryr::object_size(bfit)
#> 2.88 kB

as.numeric(pryr::object_size(bfit)/pryr::object_size(fit))*100
#> [1] 7.303713
```

Our `burgle_lm` is roughly 7.3% the size of the original `lm` object,
the iris dataset has 150 observations and 5 columns.

Another example is the using the `nycflights13::flights` dataset.

``` r
fit2 <- lm(arr_delay ~ as.factor(month) + dep_delay + origin + distance + hour, data = nycflights13::flights)
b_fit2 <- burgle(fit2)

as.numeric(pryr::object_size(b_fit2)/pryr::object_size(fit2))*100
#> [1] 0.008213793
```

Our `burgle_lm` is roughly 0.01% the size of the original `lm` object.
This dataset has 336776 observations and our model has used 5 of the 19
columns as predictors.

## Simulation Massive Example

Here one can see a simulated dataset of 10 million data points with 3
random covariates.

``` r
N <- 1e7
df <- data.frame(y = rnorm(N), x1 = runif(N), x2 = runif(N, -1, 1), x3 = runif(N, -2, 2))
mfit <- lm(y~., data = df)
b_mfit <- burgle(mfit)

m0 <- pryr::object_size(mfit)
print(m0, units = "Gb")
#> 1.60 GB
pryr::object_size(b_mfit)
#> 2.15 kB
```

The `lm` is 1.6 Gb while the `burgle_lm` object is 2152 bytes. A
reduction of size by 10^6!

## Predictions

The new predict methods for our `burgle` objects allow for one to easily
predict new values and multiple simluated responses of `newdata`. The
structure is as follows:

- The rows are indexed by the original row order given in the `newdata`
- The columns are the different sets of sampled model parameters from
  the coefficients and covariance matrix of the original model (number
  of `draws` set to 1 by default)
- If more than one simulation is done, then the simulations are items in
  a list per model and row observation (number of `sims` set to 1 by
  default)

If one wants to predict using the original model simply set
`original = TRUE`.

Depending on the model object there are different types of predictions.
By default it will return the linear predictor `(lp)`. If one wants to
see the response (`type = "response"`), which makes more sense when
using `glm`objects and survival models. The `se = FALSE` is an argument
on whether to include the standard error of the model when simluating
responses. `TRUE` means to use the model standard error when sampling.
We recommend setting it to TRUE when doing more than one simulation or
when setting `type = "response"`.

``` r
predict(bfit, newdata = head(iris), original = TRUE, draws = 1, se = FALSE, type = "lp")
#>       [,1]
#> 1 5.004788
#> 2 4.756844
#> 3 4.773097
#> 4 4.889357
#> 5 5.054377
#> 6 5.388886
predict(bfit, newdata = head(iris), original = FALSE, draws = 5, type = "lp")
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 5.056657 5.007215 4.961257 4.932543 5.050141
#> [2,] 4.790295 4.746012 4.713736 4.754813 4.785725
#> [3,] 4.822688 4.775977 4.736837 4.739272 4.814687
#> [4,] 4.917719 4.872770 4.839147 4.876992 4.915412
#> [5,] 5.109929 5.059456 5.010762 4.968089 5.103024
#> [6,] 5.454726 5.424994 5.301047 5.286944 5.422597
## These two should be similar
predict(bfit, newdata = head(iris), original = FALSE, draws = 5, sims = 5, se = TRUE, type = "response")
#> [[1]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 5.188335 5.064758 4.528114 5.460978 4.938227
#> [2,] 4.301446 4.554181 4.743173 4.829344 4.793417
#> [3,] 4.441790 4.637052 4.831230 4.536184 4.383245
#> [4,] 4.739983 4.891668 4.808410 4.820148 5.297950
#> [5,] 4.470888 5.130100 4.996598 5.327621 5.217366
#> [6,] 5.012921 5.122429 5.258801 4.947419 5.906721
#> 
#> [[2]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 4.617066 5.588269 4.633249 4.743949 5.383410
#> [2,] 4.956657 4.558627 4.992207 4.682602 4.068407
#> [3,] 4.600649 4.620536 4.568879 4.919372 4.894229
#> [4,] 4.550183 4.777452 5.177517 4.658162 5.435255
#> [5,] 5.061492 4.952346 4.724817 4.587407 4.911647
#> [6,] 5.589748 5.249421 5.258343 5.781827 4.594522
#> 
#> [[3]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 4.654951 5.018870 5.111338 4.881092 5.267062
#> [2,] 4.979906 5.620321 4.146499 5.021050 4.759204
#> [3,] 4.666988 4.154987 5.114534 4.697866 4.957075
#> [4,] 4.611058 4.369505 4.767237 4.834120 4.395112
#> [5,] 4.653835 5.552697 5.733886 4.925832 4.933249
#> [6,] 5.918499 5.859100 5.146537 5.276244 5.499586
#> 
#> [[4]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 4.957008 5.086826 5.090159 4.612207 5.024133
#> [2,] 4.917264 4.299553 4.726446 4.003881 4.799311
#> [3,] 5.164780 4.434195 5.413705 4.613030 4.858791
#> [4,] 4.609427 5.069844 4.472140 4.443499 4.939135
#> [5,] 4.441486 4.840654 5.358090 4.544883 4.997390
#> [6,] 5.557312 5.554476 5.046674 5.480583 5.295779
#> 
#> [[5]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 5.306194 4.787267 4.607056 5.086712 4.689726
#> [2,] 4.593383 4.595724 4.741960 4.961092 4.503969
#> [3,] 4.868304 5.384595 4.949947 5.243014 4.305317
#> [4,] 4.795677 4.308122 5.015002 4.419806 5.059970
#> [5,] 5.347532 5.804453 5.136792 4.602399 4.960713
#> [6,] 6.070301 5.485667 5.432137 4.758103 5.662043
```

## Generalized Linear Model

The framework should work with all `glm` family options, just the
binomial example is demonstrated below.

``` r
b_glm <- burgle(glm(I(Species == "versicolor") ~ ., family = "binomial", data = iris))

predict(b_glm, head(iris), original = FALSE, se = TRUE, draws = 5, type = "lp")
#>            [,1]       [,2]       [,3]      [,4]      [,5]
#> [1,]  1.1931513  0.8544506 -0.9125312 -4.449063 -5.371412
#> [2,]  2.2506299  1.8325844 -1.2188106 -2.859110 -5.147537
#> [3,] -1.2826411  1.7282266 -2.2889517 -2.672046 -3.255157
#> [4,] -0.5582609  0.2016295 -3.3602143 -2.508221 -1.622880
#> [5,] -2.6288233 -4.6355719 -2.9090909 -2.429126 -2.970703
#> [6,]  1.1098697 -5.8229683 -2.4733745 -3.157545 -6.473457
predict(b_glm, head(iris), original = FALSE, se = TRUE, draws = 5, type = "response")
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    1    0    0    1
#> [2,]    0    1    0    0    1
#> [3,]    1    0    0    0    1
#> [4,]    0    1    0    0    0
#> [5,]    1    0    0    0    0
#> [6,]    0    0    0    0    0
```

## Cox Proporiontal Hazards Model

``` r
library(survival)
lung <- survival::lung
lung$status <- lung$status - 1

cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno, data = lung)
cox_sm <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno, data = lung, x = FALSE, y = FALSE)

b_cox <- burgle(cox)
pryr::object_size(cox)
#> 36.02 kB
pryr::object_size(cox_sm)
#> 31.21 kB
pryr::object_size(b_cox)
#> 5.59 kB

as.numeric(pryr::object_size(b_cox)/pryr::object_size(cox))*100
#> [1] 15.52643
as.numeric(pryr::object_size(b_cox)/pryr::object_size(cox_sm))*100
#> [1] 17.91848
```

Our `burgle_coxph` model is 17.92% the size of the original Cox
proportional hazards model even after setting `x=FALSE` and `y= FALSE`.
The lung dataset has 228 observations.

One way to further reduce the size of the `burgle_coxph` is to reduce
the number of unique time points in the data, since the it contains the
baseline hazard of the model. The lung dataset as 186 unique values. If
we were to round these to the nearest 14 days that would reduce the
number of timepoints to 58.

``` r
lung$time2 <- plyr::round_any(lung$time, 14)
cox2 <- coxph(Surv(time2, status) ~ age + sex + ph.ecog + ph.karno + pat.karno, data = lung, x = TRUE, y = FALSE)
b_cox2 <- burgle(cox2)

pryr::object_size(cox2)
#> 42.50 kB
pryr::object_size(b_cox2)
#> 3.87 kB
as.numeric(pryr::object_size(b_cox2)/pryr::object_size(cox2))*100
#> [1] 9.109731
```

This reduce the size to 9.11% of the original `coxph` object.

## Survival predictions

Predictions are a slightly different structure for survival or
longitudinal models if `type = "response"` or `"risk"`, since a time
point is required. If `type = "response"` the returned results is a
simulated 1 or 0 if the event has been experienced or not at a given
time point(s). The structure is as follows:

- The rows are indexed by the original row order given in `newdata`
- The columns are the different time points at which the risk is
  calculated
- The different elements of the lists are the different sampled model
  parameters (`draws`)
- If more than one simulation is done, then the different simulations
  are returned as lists within the list element for each model (see
  below for an example example)

``` r
predict(b_cox, newdata = head(lung), original = TRUE, draws = 1, type = "lp")
#>           [,1]
#> [1,] 1.2620217
#> [2,] 0.7293038
#> [3,] 0.5927067
#> [4,] 1.4729644
#> [5,] 0.7967660
#> [6,] 0.8301417
predict(b_cox, newdata = head(lung), original = TRUE, draws = 1, type = "risk", times = 500)
#>           [,1]
#> [1,] 0.8051989
#> [2,] 0.6171886
#> [3,] 0.5672583
#> [4,] 0.8673345
#> [5,] 0.6420013
#> [6,] 0.6542671
predict(b_cox, newdata = head(lung), original = TRUE, draws = 1, type = "risk", times = c(500, 1000))
#> Warning in predict.burgle_coxph(b_cox, newdata = head(lung), original = TRUE, :
#> times has a value of 1000 which is larger than the maximum time value of 883
#>           [,1]      [,2]
#> [1,] 0.8051989 0.9851588
#> [2,] 0.6171886 0.9155426
#> [3,] 0.5672583 0.8842068
#> [4,] 0.8673345 0.9944786
#> [5,] 0.6420013 0.9289231
#> [6,] 0.6542671 0.9350234
predict(b_cox, newdata = head(lung), original = TRUE, draws = 1, type = "response", times = c(500, 1000))
#> Warning in predict.burgle_coxph(b_cox, newdata = head(lung), original = TRUE, :
#> times has a value of 1000 which is larger than the maximum time value of 883
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    0    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    1    1
#> [6,]    1    1
predict(b_cox, newdata = head(lung), original = FALSE, draws = 5, sims = 2, type = "response", times = c(500, 1000))
#> Warning in predict.burgle_coxph(b_cox, newdata = head(lung), original = FALSE,
#> : times has a value of 1000 which is larger than the maximum time value of 883
#> [[1]]
#> [[1]][[1]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    0    1
#> [6,]    1    1
#> 
#> [[1]][[2]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    0    1
#> [4,]    1    1
#> [5,]    1    1
#> [6,]    0    1
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    0    0
#> [4,]    1    1
#> [5,]    1    1
#> [6,]    1    1
#> 
#> [[2]][[2]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    1    1
#> [6,]    1    1
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    1    1
#> [6,]    1    1
#> 
#> [[3]][[2]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    1    1
#> [6,]    1    1
#> 
#> 
#> [[4]]
#> [[4]][[1]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    1    1
#> [6,]    1    1
#> 
#> [[4]][[2]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    1    1
#> [6,]    1    1
#> 
#> 
#> [[5]]
#> [[5]][[1]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    0    1
#> [6,]    1    1
#> 
#> [[5]][[2]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    0    1
#> [6,]    1    1
```

## Larger Simulation Example

``` r
## The original model at time 500
predict(b_cox, newdata = head(lung), original = TRUE, draws = 1, type = "risk", times = c(500))
#>           [,1]
#> [1,] 0.8051989
#> [2,] 0.6171886
#> [3,] 0.5672583
#> [4,] 0.8673345
#> [5,] 0.6420013
#> [6,] 0.6542671

## Doing 1000 simulations from the original model and calculating the death rate
a0 <- predict(b_cox, newdata = head(lung), original = TRUE, draws = 1, sims = 1000, type = "response", times = c(500)) |> 
  purrr::list_flatten() |>
  Reduce(f = cbind, x = _) |> 
  apply(1, mean)
a0
#> [1] 0.829 0.610 0.553 0.862 0.661 0.635

## Average survival death rate based on 1000 different models
a1 <- predict(b_cox, newdata = head(lung), original = FALSE, draws = 1000, type = "response", times = c(500)) |> 
  purrr::list_flatten() |> 
  Reduce(f = cbind, x = _) |> 
  apply(1, mean)

a1
#> [1] 0.705 0.579 0.573 0.758 0.603 0.610

## Average survival rate based on 100 simlutions for each of the 1000 models
a2 <- predict(b_cox, newdata = head(lung), original = FALSE, draws = 1000, sims = 100, type = "response", times = c(500))

### Average death per model
a3 <- lapply(a2, function(x) apply(Reduce(cbind, x), 1, mean))

## Median death rate across 1000 models and 100 simulations for each model
Reduce(rbind, a3) |> 
  apply(2, median)
#> [1] 0.810 0.625 0.570 0.870 0.645 0.660
```

This structure has also been implemented for `riskRegression::CSC` and
`flexsurv::flexsurvreg` objects and numerous others on the works and the
plan is to also incorporate `rstan` objects, and an overall
`predict.burgle_default` method and which will only a mean and
covariance matrix as inputs.
