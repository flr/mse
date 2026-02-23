# A complete set of performance statistics

A list containing a large number of performance statistics:

A list of key statistics used in fisheries management to evaluate the
performance of management procedures. This is a long list that contains
statistics that measure very similar outcomes, so a subset should be
made of those most relevant to the management objectives of interest.

## Usage

``` r
data(statistics)

statistics
```

## Format

A named list with elements of class list, each containing three
elements: a formula, a long name ('name'), and a description ('desc'),
both of type 'character':

- SB:

  SB: Mean spawner biomass

- SB0:

  SB/SB\[0\]: Mean spawner biomass relative to unfished

- minSB0:

  min(SB/SB\[0\]): Minimum spawner biomass relative to unfished

- SBMSY:

  SB/SB\[MSY\]: Mean spawnwer biomass relative to SBMSY

- F:

  F: Mean fishing mortality

- Ftarget:

  F/F\[target\]: Mean fishing mortality relative to target

- FMSY:

  F/F\[MSY\]: Mean fishing mortality relative to FMSY

- green:

  P(Green): Probability of being in Kobe green quadrant

- orange:

  P(Orange): Probability of being in Kobe orange quadrant

- yellow:

  P(Yellow): Probability of being in Kobe yellow quadrant

- red:

  P(Red): Probability of being in Kobe red quadrant

- PSBMSY:

  P(SB\>=SB\[MSY\]): Probability of SB greater or equal to SBMSY

- PSBlim:

  P(SB\>SB\[limit\]): Probability that spawner biomass is above SBlim

- PSB20B0:

  P(SB \> 0.20 %\*% SB\[0\]): Probability that spawner biomass is above
  20% SB0

- risk1:

  mean(P(SB\<B\[limit\])): ICES Risk 1, mean probability that spawner
  biomass is below Blim

- risk2:

  once(P(SB\<B\[limit\])): ICES Risk 2, probability that spawner biomass
  is above Blim once

- risk3:

  max(P(SB\>B\[limit\])): ICES Risk 3, max probability that spawner
  biomass is above Blim

- C:

  mean(C): Mean catch over years

- CMSY:

  C/MSY: Mean proportion of MSY

- AAVC:

  AAV(C): Average annual variability in catch

- IACC:

  IAC(C): Percentage inter-annual change in catch

- PC0:

  P(shutdown): Probability of fishery shutdown, defined as catch less
  than 10% of MSY

A list containing named elements, each of which represents a specific
statistic. Each of them contains:

- `formula`: A formula defining how the metric is calculated.

- `name`: A short descriptive name. This can contain
  [`plotmath()`](https://rdrr.io/r/grDevices/plotmath.html) expressions
  to be parsed by plot functions.

- `desc`: A more detailed description of the metric.

## Details

Performance statistics are used by the
[`performance()`](https://flrproject.org/mse/reference/performance.md)
method to compute time series, or aggregates along time, of quantities
of interest related to the result of applying a particular management
procedure to an operating model. They combione
[`FLCore::metrics()`](http://flrproject.org/FLCore/reference/metrics.md)
computed from the projected stocks, populations and fisheries, with
biological, economic or other reference points, but can also use the
results of calculations and decisions carried out by the MP.

Of the three elements in the list used to define each statistic, the
first unnamed element, of class 'formula' is the one evaluated by
[`performance()`](https://flrproject.org/mse/reference/performance.md).
The formula is evaluated with access to the reference points of the OM,
contained in the `refpts` slot, a set of
[FLCore::metrics](http://flrproject.org/FLCore/reference/metrics.md)
obtained from the projected OM, the contents of the
[tracking](https://flrproject.org/mse/reference/FLmse-class.md) table
with decisions and outputs from the MP internal calcultions, as well as
any function available in the workspace.

The statistics currently included are:

- SB:

  Spawner biomass in tonnes (\\SB\\), from the `SB` metric.

- SB0:

  Spawner biomass relative to unfished (\\SB/SB0\\), requires the `SB0`
  refpt.

- minSB0:

  Minimum spawner biomass relative to unfished (\\min(SB/SB0)\\)
  requires the `SB0` refpt..

- SBMSY:

  Spawner biomass relative to \\SB\[MSY\]\\ (\\SB/SB\[MSY\]\\) requires
  the `SBMSY` refpt..

- R:

  Recruitment (\\R\\), from the `R` metric.

- F:

  Fishing mortality (\\F\\), from the `F` metric.

- Ftarget:

  Fishing mortality relative to target (\\F/F\[target\]\\) requires the
  `Ftarget` refpt.

- FMSY:

  Fishing mortality relative to \\F\[MSY\]\\ (\\F/F\[MSY\]\\) requires
  the `FMSY` refpt.

- green:

  Probability of being in the Kobe green quadrant (\\P(Green)\\),
  requires the `SBMSY` and `FMSY` refpts.

- orange:

  Probability of being in the Kobe orange quadrant (\\P(Orange)\\),
  requires the `SBMSY` and `FMSY` refpts.

- yellow:

  Probability of bein, requires the `SBMSY` and `FMSY` refptsg in the
  Kobe yellow quadrant (\\P(Yellow)\\), requires the `SBMSY` and `FMSY`
  refpts.

- red:

  Probability of being in the Kobe red quadrant (\\P(Red)\\), requires
  the `SBMSY` and `FMSY` refpts.

- PSBMSY:

  Probability that spawner biomass is greater than or equal to
  \\SB\[MSY\]\\ (\\P(SB\>=SB\[MSY\])\\), requires the `SBMSY` refpt.

- PSBlim:

  Probability that spawner biomass is above \\SB\[lim\]\\
  (\\P(SB\>SB\[lim\])\\), requires the `SBlim` refpt.

- PSB20B0:

  Probability that spawner biomass is above 20% of \\SB0\\ (\\P(SB \>
  0.20 \\\*\\ SB0)\\), requires the `SB0`refpt.

- risk1:

  ICES Risk 1: Probability that spawner biomass is below \\B\[lim\]\\
  (\\P(SB\<B\[lim\])\\), requires the `SBlim` refpt.

- risk2:

  ICES Risk 2: Probability that spawner biomass falls below \\B\[lim\]\\
  at least once (\\once(P(SB\<B\[lim\]))\\), requires the `SBlim` refpt.

- risk3:

  ICES Risk 3: Maximum probability that spawner biomass is below
  \\B\[lim\]\\ (\\max(P(SB\<B\[lim\]))\\), requires the `SBlim` refpt.

- C:

  Catch in tonnes (\\C\[t\]\\), from the `C` metric.

- CMSY:

  Proportion of maximum sustainable yield (\\C/MSY\\), requires the
  `MSY` refpt.

- IACC:

  Percentage inter-annual change in catch (\\IAC(C)\\), from the `C`
  metric.

- PIACC20:

  Probability that the inter-annual change in catch being less than 20%
  (\\P(IAC(C)\<0.20)\\), from the `C` metric.

- PC0:

  Probability of fishery shutdown (\\P(shutdown)\\), defined as catch
  falling below 10% of MSY, so requires the `MSY` refpt.

## Examples

``` r
data(statistics)
# Access a specific statistic
statistics$SBMSY
#> [[1]]
#> ~SB/SBMSY
#> 
#> $name
#> [1] "SB/SB[MSY]"
#> 
#> $desc
#> [1] "Spawnwer biomass relative to SBMSY"
#> 
```
