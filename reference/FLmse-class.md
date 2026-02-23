# S4 class `FLmse`

FLmse stores the operating model, tracking object, control (mpCtrl), and
oem as used during an MSE run as well as miscellaneous args. Accessors
are provided for om, tracking, control and oem.

The `FLmse` class stores information relative to the MSE's management
procedure'.

## Usage

``` r
FLmse(...)

FLmse(...)

om(object, ...)

# S4 method for class 'FLmse'
om(object)

om(object) <- value

tracking(object, ...)

# S4 method for class 'FLmse'
tracking(object, biol = "missing")

tracking(object, ...) <- value

# S4 method for class 'FLmse'
control(object, i = NULL)

oem(object, ...)

# S4 method for class 'FLmse'
oem(object)

oem(object) <- value

# S4 method for class 'FLmse'
args(name)

# S4 method for class 'FLmse,list'
args(object) <- value
```

## Arguments

- ...:

  additional argument list that might never be used

- object:

  object of relevant class (see signature of method)

- value:

  the new object

## Details

FLmse class

Representation of a single Management Strategy Evaluation (MSE)
projection.

Methods provided on FLmse dispatch to slots in the contained OM and OEM
(e.g. stock(), catch(), ssb(), fbar(), metrics()). Use iter() to change
iteration slices for contained objects.

## Slots

- om:

  FLo. The operating model used by the simulation.

- tracking:

  FLQuants. Tracking structure used to record decisions and diagnostics.

- control:

  mpCtrl. Management procedure control used for the run.

- oem:

  FLoem. Observation error model configuration.

- args:

  list. Miscellaneous MSE arguments (iy, fy, ay, dy, data_lag,
  management_lag, etc.).

&nbsp;

- om:

  `FLom` with the operating model.

- tracking:

  `FLQuant` with record of decisions made during the mp cycle.

- args:

  `list` with assorted arguments required to run the MSE cycle.

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
if (FALSE) { # \dontrun{
# construct minimal FLmse
fm <- methods::new("FLmse")
om(fm) <- methods::new("FLo")
} # }
```
