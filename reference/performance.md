# Compute performance statistics

TODO

## Usage

``` r
# S4 method for class 'FLQuants'
performance(
  x,
  statistics = mse::statistics[c("C", "F", "SB", "AAVC")],
  refpts = FLPar(),
  years = setNames(nm = dimnames(x[[1]])$year[-1]),
  probs = NULL,
  om = NULL,
  type = NULL,
  run = NULL,
  mp = paste(c(om, type, run), collapse = "_"),
  ...
)

# S4 method for class 'FLStock'
performance(x, statistics, metrics = FLCore::metrics(x), ...)

# S4 method for class 'FLStocks'
performance(
  x,
  statistics,
  metrics = FLCore::metrics,
  refpts = FLPar(),
  years = seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear),
  probs = NULL,
  mc.cores = 1,
  ...
)

# S4 method for class 'FLom'
performance(
  x,
  refpts = x@refpts,
  statistics = mse::statistics[c("C", "F", "HR", "SB")],
  metrics = NULL,
  om = name(x),
  ...
)

# S4 method for class 'list'
performance(
  x,
  statistics,
  refpts = FLPar(),
  years = seq(dims(x[[1]])$minyear + 1, dims(x[[1]])$maxyear),
  probs = NULL,
  mc.cores = 1,
  ...
)
```

## Arguments

- statistics:

  statistics to be computed, as formula, name and description, `list`

- refpts:

  Reference points for calculations, `list`

- years:

  Years on which statistics should be computed, defaults to last year of
  input FLQuants

- run:

  Object holding the results of forward projections, as a named
  `FLQuants`

## Value

data.table Results of computing performance statistics.

## Details

Each statistics is an object of class list object, with three elements,
the first two of them compulsory:

- An unnamed element of class *formula*, e.g. `yearMeans(SB/SB0)`.

- name: A short name to be output on tables and plots, of class
  character, e.g. "SB/SB0".

- desc: A longer description of the statistics, of class character, e.g.
  "Mean spawner biomass relative to unfished"

Each statistic `formula` is evaluated against the *metrics* and *refpts*
used in the function call. Formulas can thus use (i) the names of the
`FLQuants` object or of the object returned by the call to `metrics()`,
(ii) of the *params* in the *refpts* object and, for all classes but
`FLQuants`, (iii) functions that can be called on *object*. See examples
below for the necessary matching between *metrics*, *refpts* and the
statistics formulas.

## See also

`FLQuants`

## Author

Iago Mosqueira, EC JRC

## Examples

``` r
# LOAD example FLmse object
data(sol274)
#> Warning: namespace ‘patchwork’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘dplyr’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘TMB’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘remotes’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘shiny’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘htmlwidgets’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘xtable’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘FLAssess’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘FLSRTMB’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
# GENERATE pseudo-run from last 20 years of OM
run <- window(stock(om), start=2012, end=2021)
# DEFINE statistics
statistics <- list(
  dCatch=list(~yearMeans(C[, -1]/C[, -dims(C)$year]),
    name="mean(C[t] / C[t-1])",
    desc="Mean absolute proportional change in catch"),
  varCatch=list(~yearVars(C),
    name="var(C)",
    desc="Variance in catch"),
  varF=list(~yearVars(F),
    name="var(F)",
    desc="Variance in fishing mortality"))
# COMPUTE performance
performance(run, statistics, refpts=FLPar(MSY=110000),
  metrics=list(C=catch, F=fbar), years=list(short=2016:2018, long=2016:2021))
#> Key: <statistic>
#>      statistic   year         data   iter                name
#>         <char> <char>        <num> <char>              <char>
#>   1:    dCatch   long 0.9029799916      1 mean(C[t] / C[t-1])
#>   2:    dCatch   long 0.9029799916      2 mean(C[t] / C[t-1])
#>   3:    dCatch   long 0.9029799916      3 mean(C[t] / C[t-1])
#>   4:    dCatch   long 0.9029799916      4 mean(C[t] / C[t-1])
#>   5:    dCatch   long 0.9029799916      5 mean(C[t] / C[t-1])
#>  ---                                                         
#> 596:      varF  short 0.0008390129     96              var(F)
#> 597:      varF  short 0.0060925013     97              var(F)
#> 598:      varF  short 0.0039215377     98              var(F)
#> 599:      varF  short 0.0076971982     99              var(F)
#> 600:      varF  short 0.0031926616    100              var(F)
#>                                            desc     mp
#>                                          <char> <char>
#>   1: Mean absolute proportional change in catch       
#>   2: Mean absolute proportional change in catch       
#>   3: Mean absolute proportional change in catch       
#>   4: Mean absolute proportional change in catch       
#>   5: Mean absolute proportional change in catch       
#>  ---                                                  
#> 596:              Variance in fishing mortality       
#> 597:              Variance in fishing mortality       
#> 598:              Variance in fishing mortality       
#> 599:              Variance in fishing mortality       
#> 600:              Variance in fishing mortality       
# Minimum statistic, named list with formula and name
performance(run, statistics=list(CMSY=list(~yearMeans(C/MSY), name="CMSY")),
  refpts=FLPar(MSY=110000), metrics=list(C=catch, F=fbar),
  years=list(2012:2021))
#> Key: <statistic>
#>      statistic  year      data   iter   name   desc     mp
#>         <char> <num>     <num> <char> <char> <char> <char>
#>   1:      CMSY  2021 0.1193508      1   CMSY   CMSY       
#>   2:      CMSY  2021 0.1193508      2   CMSY   CMSY       
#>   3:      CMSY  2021 0.1193508      3   CMSY   CMSY       
#>   4:      CMSY  2021 0.1193508      4   CMSY   CMSY       
#>   5:      CMSY  2021 0.1193508      5   CMSY   CMSY       
#>   6:      CMSY  2021 0.1193508      6   CMSY   CMSY       
#>   7:      CMSY  2021 0.1193508      7   CMSY   CMSY       
#>   8:      CMSY  2021 0.1193508      8   CMSY   CMSY       
#>   9:      CMSY  2021 0.1193508      9   CMSY   CMSY       
#>  10:      CMSY  2021 0.1193508     10   CMSY   CMSY       
#>  11:      CMSY  2021 0.1193508     11   CMSY   CMSY       
#>  12:      CMSY  2021 0.1193508     12   CMSY   CMSY       
#>  13:      CMSY  2021 0.1193508     13   CMSY   CMSY       
#>  14:      CMSY  2021 0.1193508     14   CMSY   CMSY       
#>  15:      CMSY  2021 0.1193508     15   CMSY   CMSY       
#>  16:      CMSY  2021 0.1193508     16   CMSY   CMSY       
#>  17:      CMSY  2021 0.1193508     17   CMSY   CMSY       
#>  18:      CMSY  2021 0.1193508     18   CMSY   CMSY       
#>  19:      CMSY  2021 0.1193508     19   CMSY   CMSY       
#>  20:      CMSY  2021 0.1193508     20   CMSY   CMSY       
#>  21:      CMSY  2021 0.1193508     21   CMSY   CMSY       
#>  22:      CMSY  2021 0.1193508     22   CMSY   CMSY       
#>  23:      CMSY  2021 0.1193508     23   CMSY   CMSY       
#>  24:      CMSY  2021 0.1193508     24   CMSY   CMSY       
#>  25:      CMSY  2021 0.1193508     25   CMSY   CMSY       
#>  26:      CMSY  2021 0.1193508     26   CMSY   CMSY       
#>  27:      CMSY  2021 0.1193508     27   CMSY   CMSY       
#>  28:      CMSY  2021 0.1193508     28   CMSY   CMSY       
#>  29:      CMSY  2021 0.1193508     29   CMSY   CMSY       
#>  30:      CMSY  2021 0.1193508     30   CMSY   CMSY       
#>  31:      CMSY  2021 0.1193508     31   CMSY   CMSY       
#>  32:      CMSY  2021 0.1193508     32   CMSY   CMSY       
#>  33:      CMSY  2021 0.1193508     33   CMSY   CMSY       
#>  34:      CMSY  2021 0.1193508     34   CMSY   CMSY       
#>  35:      CMSY  2021 0.1193508     35   CMSY   CMSY       
#>  36:      CMSY  2021 0.1193508     36   CMSY   CMSY       
#>  37:      CMSY  2021 0.1193508     37   CMSY   CMSY       
#>  38:      CMSY  2021 0.1193508     38   CMSY   CMSY       
#>  39:      CMSY  2021 0.1193508     39   CMSY   CMSY       
#>  40:      CMSY  2021 0.1193508     40   CMSY   CMSY       
#>  41:      CMSY  2021 0.1193508     41   CMSY   CMSY       
#>  42:      CMSY  2021 0.1193508     42   CMSY   CMSY       
#>  43:      CMSY  2021 0.1193508     43   CMSY   CMSY       
#>  44:      CMSY  2021 0.1193508     44   CMSY   CMSY       
#>  45:      CMSY  2021 0.1193508     45   CMSY   CMSY       
#>  46:      CMSY  2021 0.1193508     46   CMSY   CMSY       
#>  47:      CMSY  2021 0.1193508     47   CMSY   CMSY       
#>  48:      CMSY  2021 0.1193508     48   CMSY   CMSY       
#>  49:      CMSY  2021 0.1193508     49   CMSY   CMSY       
#>  50:      CMSY  2021 0.1193508     50   CMSY   CMSY       
#>  51:      CMSY  2021 0.1193508     51   CMSY   CMSY       
#>  52:      CMSY  2021 0.1193508     52   CMSY   CMSY       
#>  53:      CMSY  2021 0.1193508     53   CMSY   CMSY       
#>  54:      CMSY  2021 0.1193508     54   CMSY   CMSY       
#>  55:      CMSY  2021 0.1193508     55   CMSY   CMSY       
#>  56:      CMSY  2021 0.1193508     56   CMSY   CMSY       
#>  57:      CMSY  2021 0.1193508     57   CMSY   CMSY       
#>  58:      CMSY  2021 0.1193508     58   CMSY   CMSY       
#>  59:      CMSY  2021 0.1193508     59   CMSY   CMSY       
#>  60:      CMSY  2021 0.1193508     60   CMSY   CMSY       
#>  61:      CMSY  2021 0.1193508     61   CMSY   CMSY       
#>  62:      CMSY  2021 0.1193508     62   CMSY   CMSY       
#>  63:      CMSY  2021 0.1193508     63   CMSY   CMSY       
#>  64:      CMSY  2021 0.1193508     64   CMSY   CMSY       
#>  65:      CMSY  2021 0.1193508     65   CMSY   CMSY       
#>  66:      CMSY  2021 0.1193508     66   CMSY   CMSY       
#>  67:      CMSY  2021 0.1193508     67   CMSY   CMSY       
#>  68:      CMSY  2021 0.1193508     68   CMSY   CMSY       
#>  69:      CMSY  2021 0.1193508     69   CMSY   CMSY       
#>  70:      CMSY  2021 0.1193508     70   CMSY   CMSY       
#>  71:      CMSY  2021 0.1193508     71   CMSY   CMSY       
#>  72:      CMSY  2021 0.1193508     72   CMSY   CMSY       
#>  73:      CMSY  2021 0.1193508     73   CMSY   CMSY       
#>  74:      CMSY  2021 0.1193508     74   CMSY   CMSY       
#>  75:      CMSY  2021 0.1193508     75   CMSY   CMSY       
#>  76:      CMSY  2021 0.1193508     76   CMSY   CMSY       
#>  77:      CMSY  2021 0.1193508     77   CMSY   CMSY       
#>  78:      CMSY  2021 0.1193508     78   CMSY   CMSY       
#>  79:      CMSY  2021 0.1193508     79   CMSY   CMSY       
#>  80:      CMSY  2021 0.1193508     80   CMSY   CMSY       
#>  81:      CMSY  2021 0.1193508     81   CMSY   CMSY       
#>  82:      CMSY  2021 0.1193508     82   CMSY   CMSY       
#>  83:      CMSY  2021 0.1193508     83   CMSY   CMSY       
#>  84:      CMSY  2021 0.1193508     84   CMSY   CMSY       
#>  85:      CMSY  2021 0.1193508     85   CMSY   CMSY       
#>  86:      CMSY  2021 0.1193508     86   CMSY   CMSY       
#>  87:      CMSY  2021 0.1193508     87   CMSY   CMSY       
#>  88:      CMSY  2021 0.1193508     88   CMSY   CMSY       
#>  89:      CMSY  2021 0.1193508     89   CMSY   CMSY       
#>  90:      CMSY  2021 0.1193508     90   CMSY   CMSY       
#>  91:      CMSY  2021 0.1193508     91   CMSY   CMSY       
#>  92:      CMSY  2021 0.1193508     92   CMSY   CMSY       
#>  93:      CMSY  2021 0.1193508     93   CMSY   CMSY       
#>  94:      CMSY  2021 0.1193508     94   CMSY   CMSY       
#>  95:      CMSY  2021 0.1193508     95   CMSY   CMSY       
#>  96:      CMSY  2021 0.1193508     96   CMSY   CMSY       
#>  97:      CMSY  2021 0.1193508     97   CMSY   CMSY       
#>  98:      CMSY  2021 0.1193508     98   CMSY   CMSY       
#>  99:      CMSY  2021 0.1193508     99   CMSY   CMSY       
#> 100:      CMSY  2021 0.1193508    100   CMSY   CMSY       
#>      statistic  year      data   iter   name   desc     mp
#>         <char> <num>     <num> <char> <char> <char> <char>
# return quantiles
performance(run, statistics, refpts=FLPar(MSY=110000),
  metrics=list(C=catch, F=fbar), years=list(2012:2021),
  probs =  c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
#>     statistic  year                name
#>        <char> <num>              <char>
#>  1:    dCatch  2021 mean(C[t] / C[t-1])
#>  2:    dCatch  2021 mean(C[t] / C[t-1])
#>  3:    dCatch  2021 mean(C[t] / C[t-1])
#>  4:    dCatch  2021 mean(C[t] / C[t-1])
#>  5:    dCatch  2021 mean(C[t] / C[t-1])
#>  6:    dCatch  2021 mean(C[t] / C[t-1])
#>  7:    dCatch  2021 mean(C[t] / C[t-1])
#>  8:  varCatch  2021              var(C)
#>  9:  varCatch  2021              var(C)
#> 10:  varCatch  2021              var(C)
#> 11:  varCatch  2021              var(C)
#> 12:  varCatch  2021              var(C)
#> 13:  varCatch  2021              var(C)
#> 14:  varCatch  2021              var(C)
#> 15:      varF  2021              var(F)
#> 16:      varF  2021              var(F)
#> 17:      varF  2021              var(F)
#> 18:      varF  2021              var(F)
#> 19:      varF  2021              var(F)
#> 20:      varF  2021              var(F)
#> 21:      varF  2021              var(F)
#>     statistic  year                name
#>        <char> <num>              <char>
#>                                           desc         data  prob     mp
#>                                         <char>        <num> <num> <char>
#>  1: Mean absolute proportional change in catch 9.516750e-01  0.05       
#>  2: Mean absolute proportional change in catch 9.516750e-01  0.10       
#>  3: Mean absolute proportional change in catch 9.516750e-01  0.25       
#>  4: Mean absolute proportional change in catch 9.516750e-01  0.50       
#>  5: Mean absolute proportional change in catch 9.516750e-01  0.75       
#>  6: Mean absolute proportional change in catch 9.516750e-01  0.90       
#>  7: Mean absolute proportional change in catch 9.516750e-01  0.95       
#>  8:                          Variance in catch 5.537231e+06  0.05       
#>  9:                          Variance in catch 5.537231e+06  0.10       
#> 10:                          Variance in catch 5.537231e+06  0.25       
#> 11:                          Variance in catch 5.537231e+06  0.50       
#> 12:                          Variance in catch 5.537231e+06  0.75       
#> 13:                          Variance in catch 5.537231e+06  0.90       
#> 14:                          Variance in catch 5.537231e+06  0.95       
#> 15:              Variance in fishing mortality 5.826142e-03  0.05       
#> 16:              Variance in fishing mortality 6.563922e-03  0.10       
#> 17:              Variance in fishing mortality 8.733674e-03  0.25       
#> 18:              Variance in fishing mortality 1.000019e-02  0.50       
#> 19:              Variance in fishing mortality 1.223591e-02  0.75       
#> 20:              Variance in fishing mortality 1.408964e-02  0.90       
#> 21:              Variance in fishing mortality 1.557906e-02  0.95       
#>                                           desc         data  prob     mp
#>                                         <char>        <num> <num> <char>
# DEFINE statistics without summaries
statistics <- list(
  CMSY=list(~yearMeans(C/MSY),
    name="CMSY",
    desc="Catch over MSY"))
# COMPUTE performance
perf <- performance(run, statistics, refpts=FLPar(MSY=110000),
  metrics=list(C=catch), years=list(2012:2021))
# COMPUTE summaries
perf[, .(CMSY=mean(data))]
#>         CMSY
#>        <num>
#> 1: 0.1193508
perf <- performance(FLStocks(B=run, A=run), statistics, 
  refpts=FLPar(MSY=110000), metrics=list(C=catch), years=list(2012:2015))
```
