# Initializes a population for a given virgin biomass.

Abundances at age for a population at virgin conditions at age. An
`FLBiol` object is initiated by providing a target total biomass (`B0`)
and a value for the stock-recruit steepness (`s`). The object requires
slots to be already filled up for the mean weight-at-age (`wt`), natural
mortality (`m`), time of spawning (`spwn`) and maturity at age (`mat`).

## Usage

``` r
initiate(biol, B0, h = 0.75)
```

## Arguments

- biol:

  An `FLBiol` object to nbe initiated.

- B0:

  Initial or virgin biomass.

## Value

An updated `FLBiol` with abundances set in the first year to match the
requested biomas.

## Examples

``` r
data(ple4.biol)
#> Warning: namespace ‘dplyr’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘remotes’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘xtable’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘shiny’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
#> Warning: namespace ‘htmlwidgets’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘ple4.biol’
if (FALSE) { # \dontrun{
initiate(ple4.biol, B0=450000)
# Sets up parallel
# plan(multisession, workers=4)
initiate(propagate(ple4.biol, 100), B0=runif(100, 3e5, 5e5))
} # }
```
