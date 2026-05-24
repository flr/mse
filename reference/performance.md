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
  om = NULL,
  type = NULL,
  run = NULL,
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
  om = name(x@om),
  control = FALSE,
  type = "MP",
  run = "1",
  ...
)

# S4 method for class 'FLmses'
performance(x, ...)

# S4 method for class 'list'
performance(
  x,
  statistics,
  refpts = FLPar(),
  years = seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear),
  ...
)

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
[`FLCore::metrics()`](http://flrproject.org/FLCore/reference/metrics.md)

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
#> Key: <statistic>
#>        statistic  year       data   iter       name
#>           <char> <num>      <num> <char>     <char>
#>     1:         F  1961 0.01196100      1          F
#>     2:         F  1961 0.01284771      2          F
#>     3:         F  1961 0.01183134      3          F
#>     4:         F  1961 0.01279693      4          F
#>     5:         F  1961 0.01054540      5          F
#>    ---                                             
#> 37996:     SBMSY  2055 0.00000000     96 SB/SB[MSY]
#> 37997:     SBMSY  2055 0.00000000     97 SB/SB[MSY]
#> 37998:     SBMSY  2055 0.00000000     98 SB/SB[MSY]
#> 37999:     SBMSY  2055 0.00000000     99 SB/SB[MSY]
#> 38000:     SBMSY  2055 0.00000000    100 SB/SB[MSY]
#>                                      desc     om   type    run           mp
#>                                    <char> <char> <char> <char>       <char>
#>     1:                  Fishing mortality    ple   test    r00 ple_test_r00
#>     2:                  Fishing mortality    ple   test    r00 ple_test_r00
#>     3:                  Fishing mortality    ple   test    r00 ple_test_r00
#>     4:                  Fishing mortality    ple   test    r00 ple_test_r00
#>     5:                  Fishing mortality    ple   test    r00 ple_test_r00
#>    ---                                                                     
#> 37996: Spawnwer biomass relative to SBMSY    ple   test    r00 ple_test_r00
#> 37997: Spawnwer biomass relative to SBMSY    ple   test    r00 ple_test_r00
#> 37998: Spawnwer biomass relative to SBMSY    ple   test    r00 ple_test_r00
#> 37999: Spawnwer biomass relative to SBMSY    ple   test    r00 ple_test_r00
#> 38000: Spawnwer biomass relative to SBMSY    ple   test    r00 ple_test_r00
# Compute on OM, name taken from slot
performance(om, statistics=statistics[c("SB", "SBMSY", "F", "FMSY")],
  run="r00", type="test")
#> Key: <statistic>
#>        statistic  year       data   iter       name
#>           <char> <num>      <num> <char>     <char>
#>     1:         F  1961 0.01196100      1          F
#>     2:         F  1961 0.01284771      2          F
#>     3:         F  1961 0.01183134      3          F
#>     4:         F  1961 0.01279693      4          F
#>     5:         F  1961 0.01054540      5          F
#>    ---                                             
#> 37996:     SBMSY  2055 0.00000000     96 SB/SB[MSY]
#> 37997:     SBMSY  2055 0.00000000     97 SB/SB[MSY]
#> 37998:     SBMSY  2055 0.00000000     98 SB/SB[MSY]
#> 37999:     SBMSY  2055 0.00000000     99 SB/SB[MSY]
#> 38000:     SBMSY  2055 0.00000000    100 SB/SB[MSY]
#>                                      desc     om   type    run           mp
#>                                    <char> <char> <char> <char>       <char>
#>     1:                  Fishing mortality    PLE   test    r00 PLE_test_r00
#>     2:                  Fishing mortality    PLE   test    r00 PLE_test_r00
#>     3:                  Fishing mortality    PLE   test    r00 PLE_test_r00
#>     4:                  Fishing mortality    PLE   test    r00 PLE_test_r00
#>     5:                  Fishing mortality    PLE   test    r00 PLE_test_r00
#>    ---                                                                     
#> 37996: Spawnwer biomass relative to SBMSY    PLE   test    r00 PLE_test_r00
#> 37997: Spawnwer biomass relative to SBMSY    PLE   test    r00 PLE_test_r00
#> 37998: Spawnwer biomass relative to SBMSY    PLE   test    r00 PLE_test_r00
#> 37999: Spawnwer biomass relative to SBMSY    PLE   test    r00 PLE_test_r00
#> 38000: Spawnwer biomass relative to SBMSY    PLE   test    r00 PLE_test_r00
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
#>           om statistic      name                                         desc
#>       <char>    <char>    <char>                                       <char>
#>    1:    PLE         C     C (t)                                        Catch
#>    2:    PLE         C     C (t)                                        Catch
#>    3:    PLE         C     C (t)                                        Catch
#>    4:    PLE         C     C (t)                                        Catch
#>    5:    PLE         C     C (t)                                        Catch
#>   ---                                                                        
#> 7880:    PLE    yellow P(Yellow) Probability of being in Kobe yellow quadrant
#> 7881:    PLE    yellow P(Yellow) Probability of being in Kobe yellow quadrant
#> 7882:    PLE    yellow P(Yellow) Probability of being in Kobe yellow quadrant
#> 7883:    PLE    yellow P(Yellow) Probability of being in Kobe yellow quadrant
#> 7884:    PLE    yellow P(Yellow) Probability of being in Kobe yellow quadrant
#>        year   iter      data   type    run           mp
#>       <num> <char>     <num> <char> <char>       <char>
#>    1:  2025      1 1261.1080   test    r00 PLE_test_r00
#>    2:  2025      2  918.0387   test    r00 PLE_test_r00
#>    3:  2025      3 1753.5147   test    r00 PLE_test_r00
#>    4:  2025      4  966.0628   test    r00 PLE_test_r00
#>    5:  2025      5 1670.4657   test    r00 PLE_test_r00
#>   ---                                                  
#> 7880:  2026      1    0.8900   test    r00 PLE_test_r00
#> 7881:  2027      1    0.8700   test    r00 PLE_test_r00
#> 7882:  2028      1    0.7800   test    r00 PLE_test_r00
#> 7883:  2029      1    0.7400   test    r00 PLE_test_r00
#> 7884:  2030      1    0.7200   test    r00 PLE_test_r00
# or select a few of them
performance(mse, statistics=statistics[c("SBMSY", "FMSY")], run="r00", type="test")
#> Key: <mp, type, statistic, year>
#>           om statistic       name                               desc  year
#>       <char>    <char>     <char>                             <char> <num>
#>    1:    PLE      FMSY   F/F[MSY] Fishing mortality relative to FMSY  2025
#>    2:    PLE      FMSY   F/F[MSY] Fishing mortality relative to FMSY  2025
#>    3:    PLE      FMSY   F/F[MSY] Fishing mortality relative to FMSY  2025
#>    4:    PLE      FMSY   F/F[MSY] Fishing mortality relative to FMSY  2025
#>    5:    PLE      FMSY   F/F[MSY] Fishing mortality relative to FMSY  2025
#>   ---                                                                     
#> 1196:    PLE     SBMSY SB/SB[MSY] Spawnwer biomass relative to SBMSY  2030
#> 1197:    PLE     SBMSY SB/SB[MSY] Spawnwer biomass relative to SBMSY  2030
#> 1198:    PLE     SBMSY SB/SB[MSY] Spawnwer biomass relative to SBMSY  2030
#> 1199:    PLE     SBMSY SB/SB[MSY] Spawnwer biomass relative to SBMSY  2030
#> 1200:    PLE     SBMSY SB/SB[MSY] Spawnwer biomass relative to SBMSY  2030
#>         iter      data   type    run           mp
#>       <char>     <num> <char> <char>       <char>
#>    1:      1 1.2704782   test    r00 PLE_test_r00
#>    2:      2 0.6337901   test    r00 PLE_test_r00
#>    3:      3 1.9340616   test    r00 PLE_test_r00
#>    4:      4 0.7763312   test    r00 PLE_test_r00
#>    5:      5 1.9102496   test    r00 PLE_test_r00
#>   ---                                            
#> 1196:     96 0.7036977   test    r00 PLE_test_r00
#> 1197:     97 0.9568992   test    r00 PLE_test_r00
#> 1198:     98 1.1840236   test    r00 PLE_test_r00
#> 1199:     99 0.9837542   test    r00 PLE_test_r00
#> 1200:    100 1.0491322   test    r00 PLE_test_r00
```
