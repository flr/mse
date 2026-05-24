# sampling.oem

Samples from an operating model to obtain catch, biology and abundance
data

## Usage

``` r
sampling.oem(
  stk,
  deviances,
  observations,
  stability = 1,
  wts = TRUE,
  args,
  tracking
)
```

## Arguments

- stk:

  An FLStock object as obtained by the call to *stock(om)*.

- deviances:

  A named list of deviances, see Details.

- observations:

  A named list of observations, see Details.

- args:

  Options and arguments passed on by *mp()*.

- tracking:

  The tracking object.

## Value

A named list with elements *stk*, *idx*, *observations* and *tracking*.

## Details

This observation error model (OEM) function mimics the most common data
collection regime, in which catch-at-age and biology is sampled from the
population, and one or more indices of abundance are derived from
surveys or CPUE data.

The FLStock object passed to *sampling.oem* by the *mp* function is
simplified to match the dimensions of that present in the *observations*
slot.

## See also

[mp](https://flrproject.org/mse/reference/mp.md)

## Author

Iago Mosqueira (WUR) & Ernesto Jardim (MSC).

## Examples

``` r
data(plesim)
sampling.oem(stock(om), deviances=deviances(oem),
  observations=observations(oem),
  args=list(y0=2000, iy=2020, dy=2021, dys=2021, frq=1), tracking=FLQuant())
#> $stk
#> An object of class "FLStock"
#> 
#> Name:  
#> Description:  
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  22  1   1   1   100 
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  2000    2021    2   6   
#> 
#> Metrics: 
#>   rec: 10186 - 12063  (1000) 
#>   ssb: 6120 - 10574  (t) 
#>   catch: 1141 - 1611  (t) 
#>   fbar: 0.24 - 0.36  (f) 
#> 
#> $idx
#> An object of class "FLIndices"
#> [[1]]
#> An object of class "FLIndex"
#> 
#> Name: SUR 
#> Description: An IBTS-like survey 
#> Type :   
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  8   42  1   1   1   100 
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   8   8   1980    2021    0.5 0.5 
#> 
#> 
#> [[2]]
#> An object of class "FLIndexBiomass"
#> 
#> Name: SUR 
#> Description: An IBTS-like survey 
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  8   42  1   1   1   100 
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   8   8   1980    2021    0.5 0.5 
#> 
#> 
#> Slot "names":
#> [1] "SUR"  "CPUE"
#> 
#> Slot "desc":
#> character(0)
#> 
#> Slot "lock":
#> [1] FALSE
#> 
#> 
#> $observations
#> $observations$stk
#> An object of class "FLStock"
#> 
#> Name:  
#> Description:  
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  10  96  1   1   1   100 
#> 
#> Range:  min  max pgroup  minyear maxyear minfbar maxfbar 
#>  1   10  10  1960    2055    2   6   
#> 
#> Metrics: 
#>   rec: NA - NA  (1000) 
#>   ssb: 0 - 39518  (t) 
#>   catch: NA - NA  (t) 
#>   fbar: 0.01 - 0.81  (f) 
#> 
#> $observations$idx
#> An object of class "FLIndices"
#> [[1]]
#> An object of class "FLIndex"
#> 
#> Name: SUR 
#> Description: An IBTS-like survey 
#> Type :   
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  8   76  1   1   1   100 
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   8   8   1980    2055    0.5 0.5 
#> 
#> 
#> [[2]]
#> An object of class "FLIndexBiomass"
#> 
#> Name: SUR 
#> Description: An IBTS-like survey 
#> Distribution :   
#> Quant: age 
#> Dims:  age   year    unit    season  area    iter
#>  8   76  1   1   1   100 
#> 
#> Range:  min  max pgroup  minyear maxyear startf  endf 
#>  1   8   8   1980    2055    0.5 0.5 
#> 
#> 
#> Slot "names":
#> [1] "SUR"  "CPUE"
#> 
#> Slot "desc":
#> character(0)
#> 
#> Slot "lock":
#> [1] FALSE
#> 
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
