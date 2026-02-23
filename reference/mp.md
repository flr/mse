# mp executes a single run of a Management Procedure

An individual management procedure (MP) is run for a number of years, on
an operating model, observation error model, control and arguments.

## Usage

``` r
mp(
  om,
  oem = NULL,
  iem = NULL,
  control = ctrl,
  ctrl = control,
  args,
  scenario = "NA",
  tracking = "missing",
  verbose = !handlers(global = NA),
  progress = handlers(global = NA),
  parallel = TRUE,
  window = TRUE,
  .DEBUG = FALSE
)
```

## Arguments

- om:

  The operating model (OM), an object of class *FLom* or *FLombf*.

- oem:

  The observation error model (OEM), an object of class *FLoem*.

- iem:

  The implementation error model (IEM), an object of class *FLiem*.

- ctrl:

  A control structure for the MP run, an object of class *mpCtrl*.

- args:

  MSE arguments, *list*. Only 'iy', the intermediate (starting) year, is
  required.

- scenario:

  Name of the scenario tested in this run, *character*.

- tracking:

  Extra elements (rows) to add to the standard tracking *FLQuant* in its
  first dimensions, *character*.

- verbose:

  Should output be verbose or not, *logical*.

## Value

An object of class *FLmse*.

## Examples

``` r
# dataset contains both OM (FLom) and OEM (FLoem)
data(sol274)
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
# Set control: sa and hcr
control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=41500, target=0.27))))
# Runs mp
tes <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2034))
#> 2021  - 2022  - 2023  - 2024  - 2025  - 2026  - 2027  - 2028  - 2029  - 2030  - 2031  - 2032  - 2033  - 
# Runs mp with triannual management
tes3 <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2034, frq=3))
#> 2021  - 2024  - 2027  - 2030  - 
# Compare both runs
plot(om, list(annual=tes, triannual=tes3))
#> Warning: Removed 4500 rows containing non-finite outside the scale range
#> (`stat_fl_quantiles()`).
#> Warning: Removed 4500 rows containing non-finite outside the scale range
#> (`stat_fl_quantiles()`).
#> Warning: Removed 4500 rows containing non-finite outside the scale range
#> (`stat_fl_quantiles()`).

# 'perfect.oem' is used if none is given
tes <- mp(om, ctrl=control, args=list(iy=2021, fy=2035))
#> 2021  - 2022  - 2023  - 2024  - 2025  - 2026  - 2027  - 2028  - 2029  - 2030  - 2031  - 2032  - 2033  - 2034  - 
plot(om, tes)
#> Warning: Removed 4500 rows containing non-finite outside the scale range
#> (`stat_fl_quantiles()`).
#> Warning: Removed 4500 rows containing non-finite outside the scale range
#> (`stat_fl_quantiles()`).
#> Warning: Removed 4500 rows containing non-finite outside the scale range
#> (`stat_fl_quantiles()`).
```
