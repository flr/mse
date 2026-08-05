# Copilot Instructions for mse

This R package provides tools for constructing, running, and analyzing Management Strategy Evaluation (MSE) simulations using the FLR framework.

## Build, Test, and Lint Commands

### Building
```bash
# Full build with vignettes
make build

# Build without vignettes (faster)
make buildNV

# Install locally
make install
```

### Testing
```bash
# Run all tests
R CMD check ../mse_*.tar.gz

# Run tests from R console
library(testthat)
library(mse)
test_check("mse")

# Run specific test file
test_file("tests/testthat/test-mp.R")

# Control test suite length via environment variable
# Set MSE_TEST_SUITE="short" for quick tests (default for R CMD check)
# Set MSE_TEST_SUITE="full" for comprehensive tests
Sys.setenv(MSE_TEST_SUITE = "short")
```

### Documentation
```bash
# Update roxygen documentation
make roxygen

# Build pkgdown site
make docs
```

## Architecture

### Core MSE Workflow
The package implements a modular MSE cycle where components communicate through standardized interfaces:

1. **Operating Model (OM)**: Represents the "true" fishery system
   - `FLom`: Single-stock operating model (extends `FLo`)
   - `FLombf`: Multi-fleet biomass-based operating model (extends `FLo`)
   - Both contain stock dynamics, biological parameters, and projection methods

2. **Management Procedure (MP)**: Decision-making process defined by `mpCtrl`
   - `est`: Stock status estimator module
   - `phcr`: Parametric harvest control rule module
   - `hcr`: Harvest control rule module
   - `isys`: Implementation system module
   - `tm`: Technical measures module

3. **Observation Error Model (OEM)**: Simulates data collection (`FLoem`)
   - Generates observations from OM with specified errors
   - Creates synthetic catch and survey data

4. **Implementation Error Model (IEM)**: Simulates management implementation (`FLiem`)
   - Applies errors/deviations when implementing decisions

5. **Execution**: The `mp()` function runs a single MP; `mps()` runs multiple scenarios

### Key Classes

- **`mseCtrl`**: Base control class with `method` (function) and `args` (list) slots
  - All module specifications use this class
  - Functions are "stamped" during initialization to track provenance

- **`mpCtrl`**: List of `mseCtrl` objects defining which modules run in MP
  - Each element corresponds to a module (est, phcr, hcr, isys, tm)
  - Modules are optional; only include needed ones

- **`FLmse`**: Container for complete MSE run results
  - Slots: `om` (operating model), `tracking` (data.table), `control` (mpCtrl), `oem`, `args`
  - Methods dispatch to contained OM slots (e.g., `stock()`, `catch()`)

- **`FLo`**: Abstract parent class for operating models
  - Contains: `name`, `fleetBehaviour` (mseCtrl), `projection` (mseCtrl)
  - Extended by `FLom` and `FLombf`

### Module Dispatch Pattern
Modules follow a consistent signature pattern:
```r
module_function <- function(om, observations, args, tracking, ...) {
  # Module-specific logic
  # Returns updated/transformed inputs
}
```

Arguments come from three sources:
1. Module's `args` slot in `mseCtrl`
2. Global `args` passed to `mp()`
3. Module-specific inputs defined by module type

## Key Conventions

### S4 Class System
- All major classes use S4 with formal slots and validation
- Generic functions defined with `setGeneric()`, methods with `setMethod()`
- Accessor/replacement methods follow standard R pattern: `slot()` and `slot<-()`

### Roxygen Documentation
- Use roxygen2 markdown format (`Roxygen: list(markdown = TRUE)`)
- Templates in `man-roxygen/` directory (e.g., `Accessors.R`, `Constructors.R`)
- Include templates with `@template` tag
- Document class slots in `@slot` or `@section Slots:`
- Provide working `@examples` when possible

### File Organization (Collate Order)
Files must be loaded in the order specified in DESCRIPTION's `Collate:` field:
1. `generics.R` - Define all generic functions first
2. `*-class.R` - Class definitions and methods
3. Module implementations (mp.R, oem.R, hcr.R, etc.)
4. Utilities and plotting last

### Naming Conventions
- Module functions: `descriptor.moduletype` (e.g., `perfect.oem`, `hockeystick.hcr`)
- Classes: CamelCase with "FL" prefix for FLR classes (e.g., `FLmse`, `FLom`)
- Methods/generics: lowercase with dots (e.g., `fleet.Behaviour`, `perfect.sa`)
- Internal utilities: lowercase with underscores

### Testing Patterns
- Test files mirror source files: `test-mp.R` tests `mp.R`
- Use `test_that()` with descriptive names
- Load shared test data in `tests/testthat/setup.R` or via `local()` to avoid namespace pollution
- Context messages: `context("mp: Basic functionality (SHORT)")`
- Expect S4 classes with `expect_s4_class()`

### Parallel Execution
- MSE runs support parallel execution via `future` and `doFuture`
- Iterations split across workers automatically
- Set up with `plan()` from the `future` package
- Progress reporting via `progressr` package handlers

### License Headers
All source files should include:
```r
# Copyright European Union, YEAR
# or
# Copyright Iago MOSQUEIRA (WMR), YEAR
#
# Distributed under the terms of the EUPL-1.2
```

### Dependencies
- Core FLR packages (FLCore, FLasher, FLBRP) are required dependencies
- Additional FLR packages loaded from `https://flr.r-universe.dev`
- Use explicit package qualification when necessary for clarity
- Avoid package:: notation for FLCore functions (imported)

### Data Objects
- Test data stored in `tests/testthat/` as `.rda` files
- Package data in `data/` directory
- Raw data scripts in `data-raw/`
- Use `data()` to load package datasets in tests/examples

## Notes

- The package implements a sophisticated timing system with `iy` (initial year), `fy` (final year), `data_lag`, `management_lag`, and `frq` (frequency)
- Tracking object uses `data.table` for performance with large iteration counts
- Module functions can be debugged using custom `debug()` method on `mseCtrl` objects (requires `parallel=FALSE`)
- The `stamp.fun()` internal function adds metadata to module functions for provenance tracking
