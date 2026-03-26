# A class for an operating model (OM)

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque
eleifend odio ac rutrum luctus. Aenean placerat porttitor commodo.
Pellentesque eget porta libero. Pellentesque molestie mi sed orci
feugiat, non mollis enim tristique. Suspendisse eu sapien vitae arcu
lobortis ultrices vitae ac velit. Curabitur id

## Usage

``` r
FLom(...)

FLom(...)

# S4 method for class 'FLom'
stock(object)

# S4 method for class 'FLom,FLStock'
stock(object) <- value

# S4 method for class 'FLom'
sr(object)

# S4 method for class 'FLom,FLSR'
sr(object) <- value

# S4 method for class 'FLom,FLom'
combine(x, y, ...)

# S4 method for class 'FLmse,FLo'
om(object) <- value

# S4 method for class 'FLmse,data.table'
tracking(object) <- value

# S4 method for class 'FLmse,mpCtrl'
control(object) <- value

# S4 method for class 'FLmse,FLoem'
oem(object) <- value
```

## Arguments

- ...:

  additional argument list that might never be used

- object:

  object of relevant class (see signature of method)

- value:

  Object to assign in slot

## Slots

- `stock`:

  The population and catch history, `FLStock`.

- `sr`:

  The stock-recruitment relationship, `FLSR`.

- `refpts`:

  The estimated reference points, `FLPar`.

- `fleetBehaviour`:

  Dynamics of the fishing fleet to be used in projections, `mseCtrl`.

## Validity

- stock and sr dimensions:

  Dimensions 2:6 of the `stock` and `sr` slots must match.

- rec age:

  Stock and stock recruitment residuals must use the recruitment age.

You can inspect the class validity function by using
`getValidity(getClassDef('FLom'))`

## Accessors

All slots in the class have accessor and replacement methods defined
that allow retrieving and substituting individual slots.

The values passed for replacement need to be of the class of that slot.
A numeric vector can also be used when replacing FLQuant slots, and the
vector will be used to substitute the values in the slot, but not its
other attributes.

## Constructor

A construction method exists for this class that can take named
arguments for any of its slots. All unspecified slots are then created
to match the requirements of the class validity function.

## Methods

Methods exist for various calculations based on values stored in the
class:

- METHOD:

  Neque porro quisquam est qui dolorem ipsum.

## See also

FLComp

## Author

The FLR Team

## Examples

``` r
data(sol274)
#> Warning: namespace ‘patchwork’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘dplyr’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘TMB’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘remotes’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘shiny’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘htmlwidgets’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘xtable’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘FLAssess’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
#> Warning: namespace ‘FLSRTMB’ is not available and has been replaced
#> by .GlobalEnv when processing object ‘om’
comb <- combine(iter(om, 1:50), iter(om, 51:100))
all.equal(om, comb)
#> [1] "Attributes: < Component “refpts”: Attributes: < Component “dim”: names for target but not for current > >"
```
