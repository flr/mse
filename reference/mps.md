# Run Multiple Management Procedure Scenarios

Executes multiple runs of a Management Procedure (MP) by iterating over
a grid of values for a single module's arguments. This allows systematic
exploration of parameter sensitivity or scenario comparisons, optionally
in parallel. When no extra options are provided, a single MP run is
returned as an `FLmses` object. When options are given, results are
either collected as an `FLmses` or, if `statistics` are provided,
combined into a single `data.table` of performance statistics.

## Usage

``` r
mps(
  om,
  oem = NULL,
  iem = NULL,
  control = ctrl,
  ctrl = control,
  args,
  statistics = NULL,
  metrics = NULL,
  type = character(1),
  names = NULL,
  parallel = TRUE,
  perf = !is.null(statistics),
  ...
)
```

## Arguments

- om:

  An `FLom` object representing the Operating Model.

- oem:

  An `FLoem` object for the Observation Error Model. Defaults to `NULL`,
  in which case `perfect.oem` is used internally by
  [`mp()`](https://flrproject.org/mse/reference/mp.md).

- iem:

  An `FLiem` object for the Implementation Error Model. Defaults to
  `NULL`.

- control:

  An `mpCtrl` object defining the MP modules and their arguments. Can
  also be passed as `ctrl`.

- ctrl:

  Alias for `control`; either may be used.

- args:

  A list of MSE run arguments, including at minimum `iy` (the initial
  projection year). May also contain `seed` to set a random seed for
  reproducibility.

- statistics:

  A list of performance statistic functions to be passed to
  [`performance()`](https://flrproject.org/mse/reference/performance.md).
  Required when `perf = TRUE`.

- metrics:

  A list of metric functions passed to
  [`performance()`](https://flrproject.org/mse/reference/performance.md).
  Defaults to `NULL`.

- type:

  Character string specifying the type of performance output. Passed to
  [`performance()`](https://flrproject.org/mse/reference/performance.md).
  Defaults to `character(1)`.

- names:

  Optional character vector of names for the runs. If a single string is
  given, it is prepended to the auto-generated names. If `NULL`, names
  are constructed automatically from the module name, argument name(s),
  and rounded values.

- parallel:

  Logical. Should individual MP runs be parallelised using
  `future`/`doFuture`? Defaults to `TRUE`. Ignored if only one worker is
  available.

- perf:

  Logical. Should performance statistics be computed and returned
  instead of the full `FLmse` objects? Defaults to `TRUE` when
  `statistics` is not `NULL`.

- ...:

  A single named argument corresponding to a module present in
  `control`, whose value is a named list of argument vectors to iterate
  over. Only one module may be varied per call (e.g.
  `hcr = list(Ftarget = c(0.2, 0.3, 0.4))`). Passing more than one named
  element will raise an error.

## Value

If `perf = FALSE` (or `statistics = NULL`), an `FLmses` object
containing one `FLmse` result per parameter set. If `perf = TRUE`, a
single `data.table` combining performance statistics across all runs,
with additional columns for the varied argument values and an `mp`
identifier column.

## Details

The function iterates over all combinations of the supplied module
argument values. Arguments of unequal length are recycled to the length
of the longest. Auto-generated run names take the form
`<module>_<arg>_<value>` for a single argument, or `<arg1>-<arg2>_<i>`
for multiple arguments.

Parallel execution relies on
[`nbrOfWorkers`](https://future.futureverse.org/reference/nbrOfWorkers.html)
and `doFuture::%dofuture%`. Progress is reported either per-MP or
per-iteration depending on whether the number of runs exceeds the number
of workers.

Runs that fail are excluded from the output with a warning; if all runs
fail, an error is raised.

## See also

[`mp`](https://flrproject.org/mse/reference/mp.md),
[`performance`](https://flrproject.org/mse/reference/performance.md),
[`mpCtrl`](https://flrproject.org/mse/reference/mpCtrl-class.md),
`FLmses`

## Author

Iago Mosqueira (WMR)

## Examples

``` r
if (FALSE) { # \dontrun{
data(plesim)
control <- mpCtrl(list(
  est = mseCtrl(method = perfect.sa),
  hcr = mseCtrl(method = catchSSB.hcr, args = list(Ftarget = 0.3, Btrigger = 0))
))

# Run over a grid of Ftarget values, returning performance statistics
res <- mps(om, oem = oem, control = control, args = list(iy = 2021),
  statistics = statistics,
  hcr = list(Ftarget = c(0.2, 0.3, 0.4)))

# Run without performance statistics, returning FLmses
res <- mps(om, oem = oem, control = control, args = list(iy = 2021),
  hcr = list(Ftarget = c(0.2, 0.3, 0.4)))
} # }
```
