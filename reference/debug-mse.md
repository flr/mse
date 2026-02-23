# Debugging mse modules

Set and unset the debugging flag of a function inside the *method* slot
of a mseCtrl object.

## Usage

``` r
debug(fun, text = "", condition = NULL, signature = NULL)

# S4 method for class 'mseCtrl,missing'
debug(fun)

# S4 method for class 'mseCtrl,missing'
undebug(fun)

# S4 method for class 'mpCtrl,character'
undebug(fun, signature = NULL)

# S4 method for class 'mpCtrl,missing'
undebug(fun)

# S4 method for class 'FLo,ANY'
debug(fun)

# S4 method for class 'FLo,ANY'
undebug(fun)
```

## Arguments

- fun:

  Module or control object to debug.

- text:

  Name of module in mpCtrl.

- condition:

  Unused.

- signature:

  Name of module in mpCtrl.

## Value

Both functions invisibly return NULL

## Details

Modules in the mse control object contain the function to be called in
the *method* slot. To debug and check the behaviour of an individual
function, the *debug* method will start a browser session next time it
is called. Debugging functions requires the parallel flag to be set to
FALSE, or that no parallel backend is loaded.

Calling *undebug* on an mpCtrl without specifying a module will check
for the debugging status of each of them, and undebug if TRUE.

For objects of classes *FLom* and *FLombf*, *debug* and *undebug* will
set and unset the debugging flag on the function stored in the
*projection* slot.

## See also

[`debug`](https://rdrr.io/r/base/debug.html)

## Author

Iago Mosqueira (WMR)
