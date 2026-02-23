# S4 class `mseCtrl`

The `mseCtrl` class stores information about how a specific module will
be run. The function contained in the *method* slot will be called with
three sets of argument: those contained in the *args* slot, the *args*
argument to the call to the *mp* function, and the inputs defined by
type of module being defined by a particular object. Please see the
"Module dispatch" section in the *mse* Technical Manual.

## Usage

``` r
# S4 method for class 'mseCtrl'
initialize(.Object, ..., method, args)

method(object, ...)

# S4 method for class 'mseCtrl'
method(object)

method(object, ...) <- value

# S4 method for class 'mseCtrl,function'
method(object) <- value

args(name)

# S4 method for class 'mseCtrl'
args(name)

args(object, ...) <- value

# S4 method for class 'mseCtrl,list'
args(object) <- value

# S4 method for class 'mseCtrl'
show(object)

exists(
  x,
  where = -1,
  envir = if (missing(frame)) as.environment(where) else sys.frame(frame),
  frame,
  mode = "any",
  inherits = TRUE
)

# S4 method for class 'mseCtrl'
exists(x)

exists(
  x,
  where = -1,
  envir = if (missing(frame)) as.environment(where) else sys.frame(frame),
  frame,
  mode = "any",
  inherits = TRUE
)
```

## Arguments

- ...:

  additional argument list that might never be used

- object:

  object of relevant class (see signature of method)

## Slots

- `method`:

  The function to be run in the module call, class *function*.

- `args`:

  Arguments to be passed to the method, of class *list*.

## Accessors

All slots in the class have accessor and replacement methods defined
that allow retrieving and substituting individual slots.

The values passed for replacement need to be of the class of that slot.
A numeric vector can also be used when replacing FLQuant slots, and the
vector will be used to substitute the values in the slot, but not its
other attributes.

## Constructor

A construction method exists for this class that can take named
arguments for any of its slots. All slots are then created to match the
requirements of the class validity. If an unnamed `FLQuant` object is
provided, this is used for sizing, but not for populating any slot.

## Examples

``` r
ctl <- mseCtrl(method=function(stk, args, alpha) ssb(stk) * alpha,
  args=list(alpha=0.5))
ctl
#> Method:
#> function (stk, args, alpha) 
#> ssb(stk) * alpha
#> <environment: 0x55e1c6baeae8>
#> Arguments:
#> $alpha
#> [1] 0.5
#> 
method(ctl)
#> function (stk, args, alpha) 
#> ssb(stk) * alpha
#> <environment: 0x55e1c6baeae8>
method(ctl) <- function(stk, args, beta) ssb(stk) * beta
args(ctl)
#> $alpha
#> [1] 0.5
#> 
args(ctl) <- list(beta=0.9)
exists(ctl)
#> [1] TRUE
```
