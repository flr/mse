# Tune Management Procedures Using Bisection Method

The `tunebisect` function is designed to tune a single parameter of a
single modules in a Management Procedure (MP), typically in the Harvest
Control Rule (hcr). It uses a bisection algorithm to iteratively adjust
the parameter value unitl it achieves specified probability value for a
given management objective overn a selected time frame. For example, it
can try to find a value for the `target` argument of a `hocketstick.hcr`
that obtains a 60% mean probability of SSB being above SSBMSY over the
2032 to 2042 period.

## Usage

``` r
tunebisect(
  om,
  oem = NULL,
  control,
  statistic,
  metrics = NULL,
  args,
  tune,
  prob = 0.5,
  tol = 0.01,
  maxit = 12,
  years = ac(seq(args$iy + 1, args$fy - 1)),
  verbose = TRUE,
  ...
)
```

## Arguments

- om:

  An object representing the Operating Model, of class
  [FLom](https://flrproject.org/mse/reference/FLom-class.md) or FLombf.

- oem:

  Observation Error Model, class
  [FLoem](https://flrproject.org/mse/reference/FLoem-class.md) or
  missing, the default.

- control:

  A control object containing the settings and parameters for the
  Management Procedure, class
  [mpCtrl](https://flrproject.org/mse/reference/mpCtrl-class.md).

- statistic:

  A named list of length 1 defining the statistic used for tuning. The
  list must include:

  name

  :   The name of the statistic.

  formula

  :   The formula used to compute the statistic.

  desc

  :   A description of the statistic.

- metrics:

  Optional. A set of metrics used for performance evaluation; defaults
  to `NULL` to use the default metrics for the class of the `om` object.

- args:

  A list of arguments for running the Management Procedure. Must include
  `iy` (initial year).

- tune:

  A named list specifying the parameter of the HCR to tune and its
  range. The list must include the parameter name and the minimum and
  maximum values as a vector of length 2.

- prob:

  The target probability for tuning; defaults to `0.5`. Must be a value
  between 0 and 1.

- tol:

  Tolerance for the difference between the computed and target
  probabilities; defaults to `0.01`.

- maxit:

  Maximum number of iterations, defaults to 12.

## Details

Two initial MP runs are carried out employing the argument values
provided in `tune`. They need to return probabilities that are on both
sides of the expected one. If not, the function will return both runs
for inspection. The function should then be rerun with an wider range of
values, or the time-frame extended.

## References

Burden, Richard L.; Faires, J. Douglas (2016), "2.1 The Bisection
Algorithm", Numerical Analysis (10th ed.), Cenage Learning, ISBN
978-1-305-25366-7

## Examples

``` r
# dataset contains both OM (FLom) and OEM (FLoem)
data(plesim)
# choose sa and hcr
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=14000, target=0.18))))
# load statistics
data(statistics)
tun <- tunebisect(om, oem=oem, control=control, args=list(iy=2021, fy=2035),
tune=list(target=c(0.12, 0.32)),
metrics=list(SB=ssb), statistic=statistics['PSBMSY'], years=2025:2034)
#> [1] target: 0.12
#> [1] diff: 0.33, prob: 0.83
#> [2] target: 0.32
#> [2] diff: -0.5, prob: 0.002
#> [3] target: 0.22
#> [3] diff: -0.11, prob: 0.39
#> [4] target: 0.17
#> [4] diff: 0.25, prob: 0.75
#> [5] target: 0.195
#> [5] diff: 0.13, prob: 0.63
#> [6] target: 0.208
#> [6] diff: 0.014, prob: 0.51
#> [7] target: 0.214
#> [7] diff: -0.053, prob: 0.45
#> [8] target: 0.211
#> [8] diff: -0.017, prob: 0.48
#> [9] target: 0.209
#> [9] diff: -0.006, prob: 0.49
# Plot tuned MP
plot(om, tun)
```
