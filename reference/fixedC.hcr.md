# A fixed target C

No matter what get C = Ctrg The control argument is a list of parameters
used by the HCR.

## Usage

``` r
fixedC.hcr(stk, ctrg, args, tracking)
```

## Arguments

- stk:

  The perceived FLStock.

- control:

  A list with the element ftrg (numeric).

## Examples

``` r
# Example dataset
data(plesim)

# Sets up an mpCtrl for catch ~ MSY
ctrl <- mpCtrl(est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedC.hcr, args=list(ctrg=1500)))

# Runs mp between 2021 and 2035
run <- mp(om, control=ctrl, args=list(iy=2021, fy=2035))
#> 2021  - 2022  - 2023  - 2024  - 2025  - 2026  - 2027  - 2028  - 2029  - 2030  - 2031  - 2032  - 2033  - 2034  - 
```
