# Compute a CPUE-based indicator for use in empirical harvest control rules

`cpues.ind` computes a relative abundance indicator from one or more
CPUE or survey indices (`idx`), to be used by empirical indicator
(`ind`) modules in an `mse` Management Strategy Evaluation loop. Indices
are first restricted to a recent window of `nyears`, rescaled by
dividing by their mean over a set of reference years (`refyrs`), and
then combined across indices, using `weights`, with one of three
methods: a weighted rescaled mean (`"mean"`), a weighted mean of
standardized z-scores on the log scale (`"zscore"`), or a weighted mean
of smoothed, rescaled indices (`"smooth"`). For `combine = "mean"`, the
function additionally computes the mean level and log-linear slope of
the combined indicator over the recent window.

## Usage

``` r
cpues.ind(
  stk,
  idx,
  refyrs,
  nyears = 4,
  indices = names(idx),
  combine = c("mean", "zscore", "smooth"),
  weights = rep(1, length(indices)),
  enp.mult = 0.2,
  args,
  tracking
)
```

## Arguments

- stk:

  An `FLStock` object, returned unmodified in the output list.

- idx:

  An `FLIndices` object with the CPUE or survey indices from which the
  indicator is computed.

- refyrs:

  Reference years used to rescale (`"mean"`, `"smooth"`) or standardize
  (`"zscore"`) each index.

- nyears:

  Number of recent years, ending in `dy`, over which the indicator is
  computed. `numeric`, defaults to 4.

- indices:

  Names of the elements of `idx` to use. Defaults to `names(idx)`, i.e.
  all indices in `idx`.

- combine:

  Method used to combine the rescaled indices into a single indicator,
  one of `"mean"`, `"zscore"` or `"smooth"`.

- weights:

  Weights applied to each index, used by all three `combine` methods,
  passed on to `weighted.mean`. Defaults to equal weights, `numeric` of
  length `length(indices)`.

- enp.mult:

  Smoothing parameter passed to `smooth_index` as `enp.mult`, only used
  when `combine = "smooth"`. Defaults to 0.2.

- args:

  A `list` of MSE loop arguments, e.g. `ay` (assessment year) and `dy`
  (last data year), unpacked using `mse::spread`.

- tracking:

  An `mse` tracking object (`FLQuant`) used to store the computed
  indicator(s) for later inspection.

## Value

A `list` with three elements:

- stk:

  The input `FLStock`, unchanged.

- ind:

  An `FLQuants` with the combined indicator(s): `ind`, `mean` and
  `slope` for `combine = "mean"`; `mean` only for `combine = "zscore"`
  or `combine = "smooth"`.

- tracking:

  The updated tracking object.

## Details

`cpues.ind` is designed to be called inside an `mse` indicator (`ind`)
module, and expects `args` to contain at least the current assessment
year (`ay`) and last data year (`dy`), unpacked internally via
`mse::spread`. The mean (and, for `combine = "mean"`, the slope) of the
indicator is stored in `tracking` under `"mean.ind"` and `"slope.ind"`.

Combination across indices is done by the `weighted.mean` method for
`FLQuants`, which by default excludes `NA` values from both the weighted
sum and the sum of weights, so indices missing in a given year do not
bias the result towards zero or dilute the contribution of the indices
that do have data.

## See also

[mp](https://flrproject.org/mse/reference/mp.md),
[weighted.mean](https://rdrr.io/r/stats/weighted.mean.html),
smooth_index, zscore

## Author

Iago Mosqueira (WMR), FLR Team.

## Examples

``` r
if (FALSE) { # \dontrun{
data(ple4)
data(ple4.indices)

args <- list(ay=2017, dy=2017)
tracking <- FLQuant()

out <- cpues.ind(stk=ple4, idx=ple4.indices, refyrs=2005:2010,
  nyears=4, combine="mean", weights=c(1, 2), args=args,
  tracking=tracking)
} # }
```
