# hr

Compute partial harvest-rate estimates for each fishery in a FLombf
operating model objecta. For each fishery the function calculates the
ratio: catch (by fishery) / sum(S_f \* N \* W) where S_f is the fishery
selectivity (catch.sel), N is numbers-at-age and W are weights at age in
the population. The resulting object(s) are given units 'hr'.

## Usage

``` r
partialHR(om)
```

## Arguments

- om:

  An operating model object (typically an `FLombf` or other object for
  which `fisheries(om)` and `biol(om)` are defined).

## Value

A list of FLQuant-like objects (one per fishery) containing the partial
harvest rate with units set to 'hr'.

## Details

Compute partial harvest rates by fishery

The function sums catches across sexes/other dimensions using `unitSums`
and similarly aggregates the product of numbers, weights and selection
pattern. The returned values are per-iteration and per-unit as produced
by the underlying FL\* methods.
