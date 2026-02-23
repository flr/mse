# S4 class `FLiem`

The `FLiem` class stores the information relative to the implementation
error model of the MSE.

## Usage

``` r
# S4 method for class 'FLiem'
initialize(.Object, ...)

# S4 method for class 'FLiem'
iter(obj, iter)
```

## Arguments

- ...:

  additional argument list that might never be used

- object:

  object of relevant class (see signature of method)

## Slots

- `method`:

  The method to berun, class `function`.

- `args`:

  Arguments to be used when `method` is called, class list.

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
