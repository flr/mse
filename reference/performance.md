# Compute Performance Statistics for Management Procedure Evaluation

Evaluates the performance of a management procedure by computing
statistical metrics across simulated projections. Supports multiple
input types (FLQuants, FLStock, FLStocks, FLom, FLmse, FLmses, and
lists) and computes custom statistics defined by formulas that reference
metrics and reference points.

## Usage

``` r
# S4 method for class 'FLQuants'
performance(
  x,
  statistics = mse::statistics[c("C", "F", "SB", "AAVC")],
  refpts = FLPar(),
  years = setNames(nm = dimnames(x[[1]])$year[-1]),
  om = character(0),
  type = character(0),
  run = character(0),
  mp = paste(c(om, type, run), collapse = "_"),
  ...
)

# S4 method for class 'FLo'
performance(
  x,
  refpts = x@refpts,
  statistics = mse::statistics[c("C", "F", "HR", "SB")],
  metrics = NULL,
  om = name(x),
  ...
)

# S4 method for class 'FLombf'
performance(
  x,
  statistics = mse::statistics[c("C", "F", "HR", "SB")],
  metrics = NULL,
  om = name(x),
  ...
)

# S4 method for class 'FLmse'
performance(
  x,
  statistics = .validStatistics(om(x)),
  metrics = NULL,
  years = dimnames(om(x))$year,
  om = name(x@om),
  type = "MP",
  run = "1",
  control = FALSE,
  ...
)

# S4 method for class 'FLmses'
performance(x, type = NULL, ...)

# S4 method for class 'list'
performance(x, statistics, refpts = FLPar(), years = "missing", ...)

# S4 method for class 'FLStock'
performance(
  x,
  statistics,
  metrics = list(R = rec, SB = ssb, B = tsb, C = catch, L = landings, D = discards, F =
    fbar, HR = hr),
  ...
)

# S4 method for class 'FLStocks'
performance(x, statistics, ...)
```

## Arguments

- x:

  An object holding simulation results. Supported classes: `FLQuants`,
  `FLStock`, `FLStocks`, `FLom`, `FLmse`, `FLmses`, or `list`.

- statistics:

  A list of statistics to compute. Each element must be a named list
  with a formula and metadata (name, desc). See Details.

- refpts:

  Reference points for calculations, typically an `FLPar` object.
  Defaults to `FLPar()` (empty).

- years:

  Years on which statistics should be computed. Can be:

  - A vector of years to use

  - A named list of year vectors (names become year labels in output)
    Defaults to last year of input if omitted.

- om:

  Optional name for the operating model.

- type:

  Optional name for the MP type.

- run:

  Optional name for the model run.

- mp:

  Optional combined MP name. Auto-generated if not provided.

- ...:

  Additional arguments passed through (e.g., custom metrics, tracking
  data).

- metrics:

  Optional metrics object for FLStock/FLStocks input. Can be:

  - An `FLQuants` object with pre-computed metrics

  - A list of metric functions

  - A single function to compute metrics

- control:

  Logical. For FLmse input, include HCR control arguments in output?
  Defaults to FALSE.

- probs:

  Optional numeric vector of quantiles (0-1) to compute on statistic
  distributions across iterations. If NULL (default), returns mean
  values.

- mc.cores:

  Integer. Number of cores for parallel processing when handling lists
  or FLStocks. Defaults to 1 (sequential).

## Value

A `data.table` containing computed performance statistics with columns:

- statistic: Name of the computed statistic

- year: Year or period for which statistic was computed

- name: Display name of statistic

- desc: Description of statistic

- iter: Iteration number (or median/quantile if probs specified)

- data: The computed value

- om, type, run, mp: Identifiers for the analysis

## Details

Each statistic is defined as a named list containing:

- A formula (unnamed element) using metric and reference point names,
  e.g., `~yearMeans(SB/SB0)`

- name: Short name for tables/plots, e.g., "SB/SB0"

- desc: Longer description, e.g., "Mean spawner biomass relative to
  unfished"

Statistics formulas can reference:

1.  Names of `FLQuants` elements (metrics from estimation)

2.  Parameter names in the `refpts` object

3.  FLQuant dimension names (age, year, unit, season, area)

4.  Functions callable on the source object (for non-FLQuants input)

## See also

[statistics](https://flrproject.org/mse/reference/statistics.md),
`refpts()`,
[`metrics()`](http://flrproject.org/FLCore/reference/metrics.md)

## Author

Iago Mosqueira (WMR)

## Examples

``` r
# LOAD example FLmse object
data(plesim)
# Extract FLQuants using metrics
x <- metrics(om)
performance(x, statistics=statistics[c("SB", "SBMSY", "F", "FMSY")],
  refpts=refpts(om), om="ple", run="r00", type="test")
#>            om           mp  year statistic     name   iter         data   type
#>        <char>       <char> <num>    <char>   <char> <char>        <num> <char>
#>     1:    ple ple_test_r00  1961        SB   SB (t)      1 39511.679303   test
#>     2:    ple ple_test_r00  1961        SB   SB (t)     10 39511.679303   test
#>     3:    ple ple_test_r00  1961        SB   SB (t)    100 39511.679303   test
#>     4:    ple ple_test_r00  1961        SB   SB (t)     11 39511.679303   test
#>     5:    ple ple_test_r00  1961        SB   SB (t)     12 39511.679303   test
#>    ---                                                                        
#> 37996:    ple ple_test_r00  2055      FMSY F/F[MSY]     95     4.301398   test
#> 37997:    ple ple_test_r00  2055      FMSY F/F[MSY]     96     4.301398   test
#> 37998:    ple ple_test_r00  2055      FMSY F/F[MSY]     97     4.301398   test
#> 37999:    ple ple_test_r00  2055      FMSY F/F[MSY]     98     4.301398   test
#> 38000:    ple ple_test_r00  2055      FMSY F/F[MSY]     99     4.301398   test
#>           run                               desc
#>        <char>                             <char>
#>     1:    r00                    Spawner biomass
#>     2:    r00                    Spawner biomass
#>     3:    r00                    Spawner biomass
#>     4:    r00                    Spawner biomass
#>     5:    r00                    Spawner biomass
#>    ---                                          
#> 37996:    r00 Fishing mortality relative to FMSY
#> 37997:    r00 Fishing mortality relative to FMSY
#> 37998:    r00 Fishing mortality relative to FMSY
#> 37999:    r00 Fishing mortality relative to FMSY
#> 38000:    r00 Fishing mortality relative to FMSY
# Compute on OM, name taken from slot
performance(om, statistics=statistics[c("SB", "SBMSY", "F", "FMSY")],
  run="r00", type="test")
#>            om           mp  year statistic     name   iter         data   type
#>        <char>       <char> <num>    <char>   <char> <char>        <num> <char>
#>     1:    PLE PLE_test_r00  1961        SB   SB (t)      1 39511.679303   test
#>     2:    PLE PLE_test_r00  1961        SB   SB (t)     10 39511.679303   test
#>     3:    PLE PLE_test_r00  1961        SB   SB (t)    100 39511.679303   test
#>     4:    PLE PLE_test_r00  1961        SB   SB (t)     11 39511.679303   test
#>     5:    PLE PLE_test_r00  1961        SB   SB (t)     12 39511.679303   test
#>    ---                                                                        
#> 37996:    PLE PLE_test_r00  2055      FMSY F/F[MSY]     95     4.301398   test
#> 37997:    PLE PLE_test_r00  2055      FMSY F/F[MSY]     96     4.301398   test
#> 37998:    PLE PLE_test_r00  2055      FMSY F/F[MSY]     97     4.301398   test
#> 37999:    PLE PLE_test_r00  2055      FMSY F/F[MSY]     98     4.301398   test
#> 38000:    PLE PLE_test_r00  2055      FMSY F/F[MSY]     99     4.301398   test
#>           run                               desc
#>        <char>                             <char>
#>     1:    r00                    Spawner biomass
#>     2:    r00                    Spawner biomass
#>     3:    r00                    Spawner biomass
#>     4:    r00                    Spawner biomass
#>     5:    r00                    Spawner biomass
#>    ---                                          
#> 37996:    r00 Fishing mortality relative to FMSY
#> 37997:    r00 Fishing mortality relative to FMSY
#> 37998:    r00 Fishing mortality relative to FMSY
#> 37999:    r00 Fishing mortality relative to FMSY
#> 38000:    r00 Fishing mortality relative to FMSY
# Setup an example MSE
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.15))))
# ... and run it
mse <- mp(om, ctrl=control, args=list(iy=2025, fy=2030))
#> 2025  - 2026  - 2027  - 2028  - 2029  - 
# Compute performance using all default statistics, data(statistics)
performance(mse, run="r00", type="test")
#> Key: <mp, type, statistic, year>
#>           om           mp  year statistic      name   iter     data   type
#>       <char>       <char> <num>    <char>    <char> <char>    <num> <char>
#>    1:    PLE PLE_test_r00  2025         C     C (t)      1 1261.108   test
#>    2:    PLE PLE_test_r00  2025         C     C (t)     10 1303.160   test
#>    3:    PLE PLE_test_r00  2025         C     C (t)    100 1703.979   test
#>    4:    PLE PLE_test_r00  2025         C     C (t)     11 1402.612   test
#>    5:    PLE PLE_test_r00  2025         C     C (t)     12 1453.531   test
#>   ---                                                                     
#> 7880:    PLE PLE_test_r00  2026    yellow P(Yellow)      1    0.890   test
#> 7881:    PLE PLE_test_r00  2027    yellow P(Yellow)      1    0.870   test
#> 7882:    PLE PLE_test_r00  2028    yellow P(Yellow)      1    0.780   test
#> 7883:    PLE PLE_test_r00  2029    yellow P(Yellow)      1    0.740   test
#> 7884:    PLE PLE_test_r00  2030    yellow P(Yellow)      1    0.720   test
#>          run                                         desc
#>       <char>                                       <char>
#>    1:    r00                                        Catch
#>    2:    r00                                        Catch
#>    3:    r00                                        Catch
#>    4:    r00                                        Catch
#>    5:    r00                                        Catch
#>   ---                                                    
#> 7880:    r00 Probability of being in Kobe yellow quadrant
#> 7881:    r00 Probability of being in Kobe yellow quadrant
#> 7882:    r00 Probability of being in Kobe yellow quadrant
#> 7883:    r00 Probability of being in Kobe yellow quadrant
#> 7884:    r00 Probability of being in Kobe yellow quadrant
# or select a few of them
performance(mse, statistics=statistics[c("SBMSY", "FMSY")], run="r00", type="test")
#> Key: <mp, type, statistic, year>
#>           om           mp  year statistic       name   iter      data   type
#>       <char>       <char> <num>    <char>     <char> <char>     <num> <char>
#>    1:    PLE PLE_test_r00  2025      FMSY   F/F[MSY]      1 1.2704782   test
#>    2:    PLE PLE_test_r00  2025      FMSY   F/F[MSY]     10 1.6370809   test
#>    3:    PLE PLE_test_r00  2025      FMSY   F/F[MSY]    100 1.2342225   test
#>    4:    PLE PLE_test_r00  2025      FMSY   F/F[MSY]     11 1.4703439   test
#>    5:    PLE PLE_test_r00  2025      FMSY   F/F[MSY]     12 1.5022282   test
#>   ---                                                                       
#> 1196:    PLE PLE_test_r00  2030     SBMSY SB/SB[MSY]     95 0.6511697   test
#> 1197:    PLE PLE_test_r00  2030     SBMSY SB/SB[MSY]     96 0.7036977   test
#> 1198:    PLE PLE_test_r00  2030     SBMSY SB/SB[MSY]     97 0.9568992   test
#> 1199:    PLE PLE_test_r00  2030     SBMSY SB/SB[MSY]     98 1.1840236   test
#> 1200:    PLE PLE_test_r00  2030     SBMSY SB/SB[MSY]     99 0.9837542   test
#>          run                               desc
#>       <char>                             <char>
#>    1:    r00 Fishing mortality relative to FMSY
#>    2:    r00 Fishing mortality relative to FMSY
#>    3:    r00 Fishing mortality relative to FMSY
#>    4:    r00 Fishing mortality relative to FMSY
#>    5:    r00 Fishing mortality relative to FMSY
#>   ---                                          
#> 1196:    r00 Spawnwer biomass relative to SBMSY
#> 1197:    r00 Spawnwer biomass relative to SBMSY
#> 1198:    r00 Spawnwer biomass relative to SBMSY
#> 1199:    r00 Spawnwer biomass relative to SBMSY
#> 1200:    r00 Spawnwer biomass relative to SBMSY
```
