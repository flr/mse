# S4 class `mpCtrl`

The `mpCtrl` class defines which modules will be run my a call to the
`mp` function. It contains a series of objects of class *mseCtrl* only
for those modules required by the defined MP.

## Usage

``` r
# S4 method for class 'mpCtrl'
initialize(.Object, ...)

est(object, ...)

# S4 method for class 'mpCtrl'
est(object)

est(object) <- value

# S4 method for class 'mpCtrl,function'
est(object) <- value

phcr(object, ...)

# S4 method for class 'mpCtrl'
phcr(object)

phcr(object) <- value

# S4 method for class 'mpCtrl,function'
phcr(object) <- value

hcr(object, ...)

# S4 method for class 'mpCtrl'
hcr(object)

hcr(object) <- value

# S4 method for class 'mpCtrl,function'
hcr(object) <- value

isys(object, ...)

# S4 method for class 'mpCtrl'
isys(object)

isys(object) <- value

# S4 method for class 'mpCtrl,function'
isys(object) <- value

tm(object, ...)

# S4 method for class 'mpCtrl'
tm(object)

tm(object) <- value

# S4 method for class 'mpCtrl,function'
tm(object) <- value

# S4 method for class 'mpCtrl'
show(object)

# S4 method for class 'mpCtrl'
iters(object, iter)

# S4 method for class 'mpCtrl'
iter(obj, iter)

# S4 method for class 'mpCtrl'
method(object, element)

# S4 method for class 'mpCtrl,function'
method(object, element) <- value

# S4 method for class 'mpCtrl,function'
args(object, element) <- value

# S4 method for class 'mpCtrl,character'
debug(fun, text)
```

## Arguments

- ...:

  additional argument list that might never be used

- object:

  object of relevant class (see signature of method)

## Slots

- `est`:

  Specification for the stock status estimator, class *mseCtrl*.

- `phcr`:

  Specification for the harvest control rule parametrization, class
  *mseCtrl*.

- `hcr`:

  Specification for the harvest control rule, class *mseCtrl*.

- `isys`:

  Specification for the implementation system, class *mseCtrl*.

- `tm`:

  Specification for technical measures, class *mseCtrl*.

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
mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=41500, target=0.27))))
#> An object of class 'mpCtrl'
#> 
#> Module:  est 
#>   method: mse::perfect.sa 
#>   args: 
#> Module:  hcr 
#>   method: mse::hockeystick.hcr 
#>   args: 
#>      lim = 0, trigger = 41500, target = 0.27. 
mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr, args=list(lim=0,
  trigger=41500, target=0.27))))
#> An object of class 'mpCtrl'
#> 
#> Module:  est 
#>   method: mse::perfect.sa 
#>   args: 
#> Module:  hcr 
#>   method: mse::hockeystick.hcr 
#>   args: 
#>      lim = 0, trigger = 41500, target = 0.27. 
```
