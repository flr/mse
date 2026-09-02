# Fixed Catch Harvest Control Rule

A simple harvest control rule that sets total catch to a fixed target
value regardless of stock status or other indicators.

## Usage

``` r
fixedC.hcr(stk, ctrg, args, tracking)
```

## Arguments

- stk:

  The 'oem' observation or SA estimation, an FLStock object.

- ctrg:

  The target catch value, numeric or FLQuant. If numeric, the value is
  replicated across all iterations and management years.

- args:

  A list containing dimensionality arguments, passed on by mp(). Must
  include elements: ay (advice year), it (number of iterations), mys
  (management years vector).

- tracking:

  An FLQuant used for tracking indicators, intermediate values, and
  decisions during MP evaluation.

## Value

A list containing elements 'ctrl' (a fwdControl object) and 'tracking'.

## Details

This HCR implements a constant catch strategy where the target catch is
directly applied. The function is useful for testing fixed harvest
strategies or as a baseline reference in MSE comparisons. The target
catch can be specified as either a single numeric value (applied across
all iterations and years) or as an FLQuant object with possible year and
iteration structure.

## Examples

``` r
# Example dataset
data(plesim)

# Sets up an mpCtrl for fixed catch at MSY level
ctrl <- mpCtrl(est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedC.hcr, args=list(ctrg=1500)))

# Runs mp between 2021 and 2035
run <- mp(om, control=ctrl, args=list(iy=2021, fy=2035))
#> 2021  - 2022  - 2023  - 2024  - 2025  - 2026  - 2027  - 2028  - 2029  - 2030  - 2031  - 2032  - 2033  - 2034  - 

# Plot results
plot(om, run)


# Check output
catch(run)
#> An object of class "FLQuant"
#> iters:  100 
#> 
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   2020               2021               2022              
#>   all 1.14e+03(2.62e+02) 1.15e+03(2.78e+02) 1.09e+04(3.13e+03)
#>      year
#> age   2023               2024               2025              
#>   all 4.49e+02(1.26e+02) 1.68e+00(7.97e-01) 4.84e-06(4.39e-07)
#>      year
#> age   2026               2027               2028              
#>   all 4.89e-06(4.72e-07) 4.97e-06(5.06e-07) 4.85e-06(3.84e-07)
#>      year
#> age   2029               2030               2031              
#>   all 4.84e-06(3.33e-07) 4.89e-06(5.14e-07) 4.81e-06(4.47e-07)
#>      year
#> age   2032               2033               2034              
#>   all 4.89e-06(5.01e-07) 4.91e-06(4.37e-07) 4.84e-06(4.33e-07)
#>      year
#> age   2035              
#>   all 4.93e-06(4.94e-07)
#> 
#> units:  t 

# Using FLQuant for time-varying catch
ctrg_var <- FLQuant(seq(1000, 2000, length=10), dimnames=list(year=2021:2030))
ctrl2 <- mpCtrl(est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedC.hcr, args=list(ctrg=ctrg_var)))

# Example using function directly
fixedC.hcr(stock(om), ctrg=1500, args=list(ay=2017, it=100, 
  mys=2018:2020), tracking=FLQuant())
#> $ctrl
#> An object of class "fwdControl"
#>  (step) year quant min    value max
#>       1 2018  fbar  NA 1500.000  NA
#>       2 2019  fbar  NA 1500.000  NA
#>       3 2020  fbar  NA 1500.000  NA
#>    iters:  100 
#> 
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
