
<!-- README.md is generated from README.Rmd. Please edit that file -->

# burgle

<!-- badges: start -->
<!-- badges: end -->

The goal of burgle is to “steal” only the necessary parts of model
objects for applications in simuation.

## Installation

You can install the development version of burgle like so:

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

This structure has also been implemented for `riskRegression::CSC`
objects and the plan is to incorporate `rstan` objects, and an overall
`predict.burgle_default` method and which will only a mean and
covariance matrix as inputs.
