# setcontrol Modify on the fly the arguments on any mpCtrl module

DEFINITION

## Usage

``` r
setcontrol(control, ...)
```

## Examples

``` r
# Example dataset
data(plesim)

# Sets up an mpCtrl using hockeystick(fbar~ssb)
ctrl <- mpCtrl(est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(metric="ssb", trigger=14000, 
    output="fbar", target=0.18)))

# Runs mp between 2021 and 2035
run <- mp(om, control=ctrl, args=list(iy=2021, fy=2035))
#> 2021  - 2022  - 2023  - 2024  - 2025  - 2026  - 2027  - 2028  - 2029  - 2030  - 2031  - 2032  - 2033  - 2034  - 

# Alters the value for the 'trigger' HCR argument
run02 <- mp(om, control=setcontrol(ctrl, hcr=list(trigger=15000)),
  args=list(iy=2021, fy=2035))
#> 2021  - 2022  - 2023  - 2024  - 2025  - 2026  - 2027  - 2028  - 2029  - 2030  - 2031  - 2032  - 2033  - 2034  - 

# Plots results
plot(om, list(T14k=run, T15k=run02))
```
