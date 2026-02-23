# hr method for FLombf

S4 method for `hr(object)` when `object` is an `FLombf`. The method sums
partial harvest-rate contributions across fisheries to produce a total
harvest-rate estimate.

## Usage

``` r
# S4 method for class 'FLombf'
hr(object)
```

## Arguments

- object:

  An `FLombf` object.

## Value

An FLQuant (or aggregated FLQuant) that is the sum of the partial
harvest rates across fisheries.
