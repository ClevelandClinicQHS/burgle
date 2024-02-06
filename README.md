
<!-- README.md is generated from README.Rmd. Please edit that file -->

# burgle

<!-- badges: start -->
<!-- badges: end -->

The goal of burgle is to “steal” only the necessary parts of model
objects.

## Installation

You can install the development version of burgle like so:

``` r
devtools::install_github("ClevelandClinicQHS/burgle")
```

## Pros of `burgle`

1.  The reduction in size can save on memory and storage space.
    Something that is sometimes in great demand since R works with local
    active memory.
2.  The removal of data from the model objects can allow them to be
    shared freely, if there are data sharing concerns or requirements.
3.  A streamline to simulate response for parameter uncertainty and
    probabilistic sampling

## Linear Model Example

``` r

set.seed(287453)
library(burgle)
fit <- lm(Sepal.Length ~., data = iris)
bfit <- burgle(fit)
object.size(fit)
#> 68776 bytes
object.size(bfit)
#> 5296 bytes

as.numeric(object.size(bfit)/object.size(fit))*100
#> [1] 7.700361
```

Our `burgle_lm` is roughly 7.7% the size of the original `lm` object,
the iris dataset has 150 observations and 5 columns.

Another example is the using the `nycflights13::flights` dataset.

``` r
fit2 <- lm(arr_delay ~ as.factor(month) + dep_delay + origin + distance + hour, data = nycflights13::flights)
b_fit2 <- burgle(fit2)

as.numeric(object.size(b_fit2)/object.size(fit2))*100
#> [1] 0.008576956
```

Our `burgle_lm` is roughly 0.01% the size of the original `lm` object.
This dataset has 336776 observations and our model has used 5 of the 19
columns as predictors.

## Prediction

The new predict methods for our `burgle` objects allow for one to easily
predict using `newdata`. The structure is as follows:

- The rows are indexed by the original row order given in the `newdata`
- The columns are the different models (number of `draws` set to 1 by
  default)
- If more than one simulation is done, then the simulations are items in
  a list per model and row observation (number of `sims` set to 1 by
  default)

## Predictions

If one wants to predict using the original model simply set
`original = TRUE`.

Depending on the model object there are different types of predictions.
By default it will return the linear predictor `(lp)`. If one wants to
see the response, which makes more sense when looking at `glm` and
survival models, one can set the `type = "response"`. The `se = FALSE`
is an argument on whether to include the standard error of the model
when simluating responses. `TRUE` means to use the model standard error.
We recommend setting it to FALSE when doing more than one simulation or
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
#> [1,] 4.946335 5.003012 5.039852 5.037592 5.041084
#> [2,] 4.745889 4.765881 4.859152 4.774510 4.794691
#> [3,] 4.738647 4.771583 4.837747 4.806213 4.811356
#> [4,] 4.873399 4.902457 4.988977 4.900656 4.925862
#> [5,] 4.986424 5.050438 5.075992 5.090209 5.090363
#> [6,] 5.352348 5.347911 5.370655 5.431830 5.435649
## These two should be similar
predict(bfit, newdata = head(iris), original = FALSE, draws = 5, se = TRUE, type = "response")
#> [[1]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 4.949364 4.611666 4.845867 4.613093 5.025597
#> [2,] 4.848257 4.800414 4.404981 4.116747 4.936797
#> [3,] 4.737599 4.783998 4.815853 4.977797 4.894802
#> [4,] 5.133902 4.627760 4.859834 4.680757 4.928082
#> [5,] 5.575770 5.134491 5.422847 5.052830 5.132309
#> [6,] 5.062045 5.713390 5.728405 5.831163 5.637258
predict(bfit, newdata = head(iris), original = FALSE, draws = 5, sims = 5, se = TRUE, type = "response")
#> [[1]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 5.260501 5.037132 4.593463 4.882995 5.187388
#> [2,] 4.538671 4.815121 4.157185 5.004187 4.726650
#> [3,] 4.923483 5.116510 4.683396 5.430673 4.685962
#> [4,] 4.737263 5.091307 5.170321 4.829057 5.302072
#> [5,] 5.188184 4.935192 4.805214 5.321894 5.333787
#> [6,] 5.350631 4.817877 5.214976 5.668268 5.239573
#> 
#> [[2]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 4.914411 5.222610 4.731044 5.074234 5.153334
#> [2,] 4.846126 4.951256 4.441396 5.022446 5.075101
#> [3,] 4.917130 5.326907 5.104704 5.156157 5.021481
#> [4,] 4.894094 5.002437 5.112773 4.995279 4.929961
#> [5,] 4.587194 5.053612 5.358197 4.944248 4.500509
#> [6,] 5.582703 5.016379 4.990239 6.195810 5.191818
#> 
#> [[3]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 5.224223 4.918758 5.306746 5.192917 4.872313
#> [2,] 5.366040 4.435256 4.089466 5.307186 4.646172
#> [3,] 4.632594 4.820303 4.397087 4.468342 5.424017
#> [4,] 4.529874 5.492531 5.170977 5.377690 4.720718
#> [5,] 4.808205 4.782066 5.274488 5.219686 4.943599
#> [6,] 5.408754 5.618188 5.730797 5.089727 5.780761
#> 
#> [[4]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 5.284213 4.557900 5.596457 5.125196 5.285726
#> [2,] 4.769331 4.779539 4.543296 4.590669 4.466377
#> [3,] 5.040305 4.732371 4.914880 4.875975 4.289284
#> [4,] 5.105487 5.272259 4.914411 5.149757 4.762387
#> [5,] 5.522158 5.475654 4.687516 5.669193 4.884984
#> [6,] 5.519946 5.359038 5.575087 5.662173 5.358181
#> 
#> [[5]]
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 5.061267 4.759573 5.553449 5.275785 4.989205
#> [2,] 5.110294 4.719642 4.792234 4.770306 4.354107
#> [3,] 4.856919 5.051506 4.813532 4.566699 4.952839
#> [4,] 4.915484 5.147481 4.861465 4.217409 5.137209
#> [5,] 5.304423 5.320645 5.149594 5.878196 4.950783
#> [6,] 5.615928 5.782897 5.578978 5.588136 5.978252
```

## Generalized Linear Model

The framework should work with all `glm` family options, just the
binomial one is demonstrated below.

``` r
b_glm <- burgle(glm(I(Species == "versicolor") ~ ., family = "binomial", data = iris))

predict(b_glm, head(iris), original = FALSE, se = TRUE, draws = 5, type = "lp")
#>            [,1]       [,2]       [,3]       [,4]      [,5]
#> [1,] -1.3400903 -1.9636511 -1.9390781 -1.3066577 -2.377785
#> [2,] -0.8799525 -0.9429721 -0.7524696 -0.1854923 -1.017676
#> [3,] -0.9028540 -1.5028075 -1.0900159 -0.7167104 -1.648365
#> [4,] -0.3946237 -0.9196176 -0.2487582 -0.3693506 -1.113291
#> [5,] -1.2653576 -2.1506422 -1.9781124 -1.5404598 -2.629537
#> [6,] -1.8966961 -3.0575047 -3.1887784 -2.3715294 -3.596380
predict(b_glm, head(iris), original = FALSE, se = TRUE, draws = 5, type = "response")
#> [[1]]
#>             [,1]         [,2]         [,3]        [,4]        [,5]
#> [1,] 0.002972337 0.0796571307 0.0594553178 0.004916541 0.010680913
#> [2,] 0.643708189 0.3504714765 0.5715759062 0.011141659 0.109030014
#> [3,] 0.245415778 0.3984911836 0.0006268148 0.389655001 0.049463538
#> [4,] 0.064650704 0.0228844928 0.1374993677 0.091498976 0.046233798
#> [5,] 0.143923090 0.1188834747 0.5257984809 0.182858444 0.007538730
#> [6,] 0.048304493 0.0002524787 0.2543708047 0.459963852 0.001325096
```

## Cox Proporiontal Hazards Model

``` r
library(survival)
lung <- survival::lung
lung$status <- lung$status - 1

cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno, data = lung)
cox_sm <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno, data = lung, x = FALSE, y = FALSE)

b_cox <- burgle(cox)
object.size(cox)
#> 56400 bytes
object.size(cox_sm)
#> 37760 bytes
object.size(b_cox)
#> 8184 bytes

as.numeric(object.size(b_cox)/object.size(cox))*100
#> [1] 14.51064
as.numeric(object.size(b_cox)/object.size(cox_sm))*100
#> [1] 21.67373
```

Our `burgle_coxph` model is 21.67% the size of the original Cox
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

object.size(cox2)
#> 62048 bytes
object.size(b_cox2)
#> 6464 bytes
as.numeric(object.size(b_cox2)/object.size(cox2))*100
#> [1] 10.41774
```

This reduce the size to 10.42% of the original `coxph` object.

## Survival predictions

Predictions are a slightly different structure if `type = "response"` or
`"risk"`, since a time point is required. If `type = "response"` the
returned results is a simulated 1 or 0 if the event has been experienced
or not at a given time point. The structure is as follows: \* The rows
are indexed by the original row order given in `newdata` \* The columns
are the different time points at which the risk is calculated \* The
different elements of the lists are the different models (`draws`) \* If
more than one simulation is done, then the different simulations are
returned as list within the list element for each model (see below for
an example example)

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
#> [[1]]
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
#> [[1]]
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
#> [[1]]
#> [[1]][[1]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1
#> [4,]    1    1
#> [5,]    0    1
#> [6,]    1    1
predict(b_cox, newdata = head(lung), original = FALSE, draws = 5, sims = 2, type = "risk", times = c(500, 1000))
#> Warning in predict.burgle_coxph(b_cox, newdata = head(lung), original = FALSE,
#> : times has a value of 1000 which is larger than the maximum time value of 883
#> [[1]]
#>           [,1] [,2]
#> [1,] 1.0000000    1
#> [2,] 0.9999951    1
#> [3,] 0.9998165    1
#> [4,] 1.0000000    1
#> [5,] 0.9999984    1
#> [6,] 0.9998554    1
#> 
#> [[2]]
#>           [,1]      [,2]
#> [1,] 0.4704663 0.8053183
#> [2,] 0.4393519 0.7744982
#> [3,] 0.3607687 0.6839324
#> [4,] 0.5528842 0.8740465
#> [5,] 0.4078638 0.7404433
#> [6,] 0.4742835 0.8089101
#> 
#> [[3]]
#>           [,1]      [,2]
#> [1,] 0.9206248 0.9985281
#> [2,] 0.6701926 0.9424504
#> [3,] 0.6102466 0.9115440
#> [4,] 0.8397809 0.9910257
#> [5,] 0.6532296 0.9345203
#> [6,] 0.8478557 0.9921441
#> 
#> [[4]]
#>           [,1]      [,2]
#> [1,] 0.7632795 0.9754905
#> [2,] 0.5295309 0.8564106
#> [3,] 0.4870341 0.8206126
#> [4,] 0.7533528 0.9727570
#> [5,] 0.5294700 0.8563627
#> [6,] 0.6735696 0.9439549
#> 
#> [[5]]
#>           [,1]      [,2]
#> [1,] 0.9718461 0.9998978
#> [2,] 0.8544097 0.9929859
#> [3,] 0.8184430 0.9876188
#> [4,] 0.9553268 0.9996648
#> [5,] 0.8784128 0.9955886
#> [6,] 0.8405274 0.9911329
```

## Larger Example

``` r
## The original model at time 500
predict(b_cox, newdata = head(lung), original = TRUE, draws = 1, type = "risk", times = c(500))
#> [[1]]
#>           [,1]
#> [1,] 0.8051989
#> [2,] 0.6171886
#> [3,] 0.5672583
#> [4,] 0.8673345
#> [5,] 0.6420013
#> [6,] 0.6542671

## Doing 1000 simulations from the original model and calculating the death rate
a0 <- predict(b_cox, newdata = head(lung), original = TRUE, draws = 1, sims = 1000, type = "response", times = c(500)) |> purrr::list_flatten() |> Reduce(f = cbind, x = _) |> apply(1, mean)
a0
#> [1] 0.785 0.609 0.564 0.860 0.653 0.659

## Average survival death rate based on 1000 different models
a1 <- predict(b_cox, newdata = head(lung), original = FALSE, draws = 1000, type = "response", times = c(500)) |> purrr::list_flatten() |> Reduce(f = cbind, x = _) |> apply(1, mean)

a1
#> [1] 0.707 0.611 0.583 0.772 0.623 0.651

## Average survival rate based on 100 simlutions for each of the 1000 models
a2 <- predict(b_cox, newdata = head(lung), original = FALSE, draws = 1000, sims = 100, type = "response", times = c(500))

### Average death per model
a3 <- lapply(a2, function(x) apply(Reduce(cbind, x), 1, mean))

a4 <- Reduce(rbind, a3)

apply(a4, 2, median)
#> [1] 0.785 0.600 0.550 0.860 0.620 0.645
```

This structed has also been implemented for
`riskRegression::CSC objects` and the plan is to incorportate `rstan`
objects, and an overall default method and which will require a mean and
covariance matrix as inputs.
