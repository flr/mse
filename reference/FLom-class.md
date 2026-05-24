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
data(plesim)
comb <- combine(iter(om, 1:50), iter(om, 51:100))
all.equal(om, comb)
#>  [1] "Attributes: < Component “sr”: Attributes: < Component “fitted”: Attributes: < Component “dim”: Mean relative difference: 99 > > >"                        
#>  [2] "Attributes: < Component “sr”: Attributes: < Component “fitted”: Numeric: lengths (98, 9800) differ > >"                                                   
#>  [3] "Attributes: < Component “sr”: Attributes: < Component “params”: Attributes: < Component “dim”: Mean relative difference: 1 > > >"                         
#>  [4] "Attributes: < Component “sr”: Attributes: < Component “params”: Numeric: lengths (3, 6) differ > >"                                                       
#>  [5] "Attributes: < Component “sr”: Attributes: < Component “rec”: Attributes: < Component “dim”: Mean relative difference: 99 > > >"                           
#>  [6] "Attributes: < Component “sr”: Attributes: < Component “rec”: Numeric: lengths (98, 9800) differ > >"                                                      
#>  [7] "Attributes: < Component “sr”: Attributes: < Component “ssb”: Attributes: < Component “dim”: Mean relative difference: 99 > > >"                           
#>  [8] "Attributes: < Component “sr”: Attributes: < Component “ssb”: Numeric: lengths (98, 9800) differ > >"                                                      
#>  [9] "Attributes: < Component “sr”: Attributes: < Component “vcov”: Lengths: 4, 8 > >"                                                                          
#> [10] "Attributes: < Component “sr”: Attributes: < Component “vcov”: Attributes: < Component “dim”: Numeric: lengths (2, 3) differ > > >"                        
#> [11] "Attributes: < Component “sr”: Attributes: < Component “vcov”: Attributes: < Component “dimnames”: names for current but not for target > > >"             
#> [12] "Attributes: < Component “sr”: Attributes: < Component “vcov”: Attributes: < Component “dimnames”: Length mismatch: comparison on first 2 components > > >"
#> [13] "Attributes: < Component “sr”: Attributes: < Component “vcov”: target is matrix, current is array > >"                                                     
```
