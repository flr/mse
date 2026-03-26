# A fixed target F

No matter what get F = ftrg The control argument is a list of parameters
used by the HCR.

## Usage

``` r
fixedF.hcr(stk, ftrg, args, tracking)
```

## Arguments

- stk:

  The perceived FLStock.

- control:

  A list with the element ftrg (numeric).

## Examples

``` r
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
fixedF.hcr(stock(om), ftrg=0.13, args=list(ay=2017, management_lag=1,
  frq=1), tracking=FLQuant())
#> $ctrl
#> An object of class "fwdControl"
#>  (step) year quant min value max
#>       1 2018  fbar  NA 0.130  NA
#> 
#> $tracking
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> quant 1 
#>   all NA
#> 
#> units:  NA 
#> 
```
