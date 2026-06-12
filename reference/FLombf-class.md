# Append two FLombf objects along the year dimension

Joins two `FLombf` objects along the year axis: the first object is
windowed to `after`, and the second from `after + 1` to its last year.
The two windowed pieces are then combined using `FLCore::append` on the
underlying `FLBiols` and `FLFisheries` slots.

## Usage

``` r
FLombf(...)

# S4 method for class 'FLombf,FLombf'
append(x, values, after = dims(values)$minyear)
```

## Arguments

- ...:

  additional argument list that might never be used

- x:

  An `FLombf` object, used up to year `after`.

- values:

  An `FLombf` object, used from year `after + 1`.

- after:

  Integer year. The last year taken from `x`. Defaults to the last year
  of `x`.

- object:

  object of relevant class (see signature of method)

## Value

A new `FLombf` object spanning the years of `x` up to `after` joined
with the years of `values` from `after + 1`.

## Examples

``` r
data(plesim)
# Split an FLombf at year 2020 and re-join
om1 <- window(om, end=2020)
om2 <- window(om, start=2021)
om3 <- append(om1, om2, after=2020)
```
