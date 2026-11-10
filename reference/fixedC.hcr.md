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
#> age   2020           2021           2022           2023          
#>   all 1141(2.62e+02) 1153(2.78e+02) 1500(4.06e-08) 1500(2.49e-08)
#>      year
#> age   2024           2025           2026           2027          
#>   all 1500(4.97e-08) 1500(9.89e-09) 1500(7.68e-08) 1500(1.04e-07)
#>      year
#> age   2028           2029           2030           2031          
#>   all 1500(1.31e-07) 1500(3.66e-08) 1500(7.80e-08) 1500(8.18e-08)
#>      year
#> age   2032           2033           2034           2035          
#>   all 1500(4.13e-07) 1500(3.17e-07) 1500(7.73e-07) 1500(2.06e-06)
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
#>       1 2018 catch  NA 1500.000  NA
#>       2 2019 catch  NA 1500.000  NA
#>       3 2020 catch  NA 1500.000  NA
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
