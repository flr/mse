# Select and/or Compute a Metric from the *stk* and *ind* inputs

A metric, defined here as a time series, commonly age-aggregated, is
computed or extracted from the input FLStock (*stk*) and FLQuants
(*ind*). These have been returned by the call to the *est*inmation step
in a call to [`mp()`](https://flrproject.org/mse/reference/mp.md).

## Usage

``` r
selectMetric(metric = "missing", stk, ind, ...)
```

## Arguments

- metric:

  A metric to use, which can be one of the following:

  - `missing`: Defaults to the first element of the `ind` object if only
    one element is present.

  - `character`: The name of a metric in `ind` to extract, or the name
    of a function to compute the metric.

  - `function`: A function to compute the metric with *stk* as input.

- stk:

  The stock object, used for computing the metric (if applicable).

- ind:

  A FLQuants object containing potential metrics, used for extraction
  based on `metric`.

- ...:

  Additional arguments passed to the function `metric` (if it is a
  function or callable).

## Value

The selected or computed metric, either extracted from `ind` or computed
using `stk` and `metric`.

## Details

If *metric* is a character string and matches a name in *ind*, then that
*FLQuant* element is returned. Otherwise, or if *metric* is a function,
it is called on *stk*. See examples below.

## Author

Iago Mosqueira (WUR)

## Examples

``` r
data(ple4)
# Computes 'catch' metric from 'stk', 'ind' is empty
selectMetric("catch", stk=ple4, ind=FLQuants())
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all  78360  88785 105186 117975 119541 126290 140815 147540 151408 162266
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 154474 149820 146178 136619 141226 149390 151515 157994 165392 175881
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 165843 178166 172652 184690 184494 192439 212632 228265 247071 279228
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 308480 315245 292035 250604 218184 192691 179573 151248 132629 131719
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 152195 171240 170662 145998 128107 143807 154029 140056 114551 111864
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 104770 113397 115703 118824 119718 131872 141055 139750 137338 131216
#>      year
#> age   2017  
#>   all 124922
#> 
#> units:  t 

# Computes own rfunction (ratio of discards to landings) as metric from 'stk'
selectMetric(function(x) discards(x) / landings(x), stk=ple4, ind=FLQuants())
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957  1958  1959  1960  1961  1962  1963  1964  1965  1966  1967  1968 
#>   all 0.105 0.197 0.345 0.329 0.402 0.398 0.365 0.328 0.436 0.650 0.486 0.238
#>      year
#> age   1969  1970  1971  1972  1973  1974  1975  1976  1977  1978  1979  1980 
#>   all 0.192 0.222 0.204 0.145 0.133 0.372 0.751 0.440 0.528 0.388 0.441 0.226
#>      year
#> age   1981  1982  1983  1984  1985  1986  1987  1988  1989  1990  1991  1992 
#>   all 0.219 0.321 0.480 0.403 0.355 0.676 0.990 0.875 0.556 0.437 0.476 0.430
#>      year
#> age   1993  1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004 
#>   all 0.266 0.199 0.209 0.407 0.836 1.307 0.753 0.455 0.924 0.650 1.060 0.697
#>      year
#> age   2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#>   all 0.863 0.800 0.755 0.756 0.681 0.600 0.608 0.602 0.452 0.617 0.509 0.519
#>      year
#> age   2017 
#>   all 0.474
#> 
#> units:   

# Returns 'catch' metric from 'ind' (defined as log), takes precedence over 'stk'
selectMetric("catch", stk=ple4, ind=FLQuants(catch=log(catch(ple4))))
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970
#>   all 11.3 11.4 11.6 11.7 11.7 11.7 11.9 11.9 11.9 12.0 11.9 11.9 11.9 11.8
#>      year
#> age   1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984
#>   all 11.9 11.9 11.9 12.0 12.0 12.1 12.0 12.1 12.1 12.1 12.1 12.2 12.3 12.3
#>      year
#> age   1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998
#>   all 12.4 12.5 12.6 12.7 12.6 12.4 12.3 12.2 12.1 11.9 11.8 11.8 11.9 12.1
#>      year
#> age   1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
#>   all 12.0 11.9 11.8 11.9 11.9 11.8 11.6 11.6 11.6 11.6 11.7 11.7 11.7 11.8
#>      year
#> age   2013 2014 2015 2016 2017
#>   all 11.9 11.8 11.8 11.8 11.7
#> 
#> units:   

# Any function available for 'stk' works
selectMetric(ssb, stk=ple4, ind=FLQuants())
#> An object of class "FLQuant"
#> , , unit = unique, season = all, area = unique
#> 
#>      year
#> age   1957   1958   1959   1960   1961   1962   1963   1964   1965   1966  
#>   all 342223 355375 362119 380052 391386 482245 440658 430475 383583 404516
#>      year
#> age   1967   1968   1969   1970   1971   1972   1973   1974   1975   1976  
#>   all 473938 458977 402865 370472 361610 366129 302365 298094 301757 328726
#>      year
#> age   1977   1978   1979   1980   1981   1982   1983   1984   1985   1986  
#>   all 329116 327544 302271 319090 290780 284211 339238 367251 394666 407972
#>      year
#> age   1987   1988   1989   1990   1991   1992   1993   1994   1995   1996  
#>   all 470952 424696 448230 396458 356947 311431 279962 233481 222203 203391
#>      year
#> age   1997   1998   1999   2000   2001   2002   2003   2004   2005   2006  
#>   all 204948 237863 219149 230902 234211 221902 248312 233478 253737 284447
#>      year
#> age   2007   2008   2009   2010   2011   2012   2013   2014   2015   2016  
#>   all 293330 371837 453026 554245 575459 617539 709948 823276 774157 836453
#>      year
#> age   2017  
#>   all 913290
#> 
#> units:  t 
```
