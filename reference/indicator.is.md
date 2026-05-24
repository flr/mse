# Indicator Implementation System Module

Applies the harvest control rule (HCR) decision to scale the target
catch or fishing mortality based on a reference quantity (status-quo
fishing mortality or catch).

## Usage

``` r
indicator.is(stk, ctrl, args, tracking, system = c("output", "input"), ...)
```

## Arguments

- stk:

  The perceived FLStock object returned by the OEM module.

- ctrl:

  The fwdControl object output by the *hcr* step, containing the HCR
  decision.

- args:

  The MSE run arguments, including `sqy` (status-quo years).

- tracking:

  The tracking object for recording module decisions and outputs.

- system:

  Character, either "output" (default) or "input". If "output", catch is
  used as reference; if "input", fishing mortality is used.

- ...:

  Additional arguments (currently unused).

## Value

A list containing:

- ctrl:

  The modified fwdControl with scaled target values.

- tracking:

  The updated tracking object.

## Details

This implementation system (IS) function adjusts the management decision
from the HCR step by multiplying it with a reference quantity computed
from the stock-at-age data in the perceived stock, allowing for
indicator-based management approaches. The reference quantity can be
either the status-quo fishing mortality (input system) or catch (output
system).

When `system="output"`, the function scales the HCR target by the mean
catch from the status-quo years. When `system="input"`, it scales by the
mean fishing mortality. This approach allows MPs to work with relative
rather than absolute targets.

## See also

[`tac.is`](https://flrproject.org/mse/reference/tac.is.md), `sp.is`,
`seasonal.is`

## Author

Ernesto Jardim, Iago Mosqueira

## Examples

``` r
data(plesim)
# Setup control with indicator.is using output system (catch-based)
control <- mpCtrl(list(est=mseCtrl(method=perfect.sa),
  hcr=mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=0.5, target=1.2)),
  isys=mseCtrl(method=indicator.is, args=list(system="output"))))
# Run MP
run <- mp(om, oem, control=control, args=list(iy=2021, fy=2027))
#> 2021  - 2022  - 2023  - 2024  - 
#> Warning: Selected elements do not form a coherent 6D array
#> 2025  - 
#> Warning: Selected elements do not form a coherent 6D array
#> 2026  - 
#> Warning: Selected elements do not form a coherent 6D array
#> 
```
