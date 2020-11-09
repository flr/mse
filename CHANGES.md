---
title:
author: 
tags:
---

- FLomBF class composed of
  - biols (*FLBiols*)
  - fisheries (*FLFisheries*)
  - refpts (*FLPars*)

- Parent class *FLo* from which both *FLom* and *FLOmBF* derive.

- goFish turned into a method for *FLom* or *FLomBF*.

- oem methods now take as input the full om, rathee than stk (*FLStock*).

- TODO tracking is now by biols, stored in 'units'. No tracking by fishery.

# PROPOSALS

- CHANGE est to output FLStock  or FLQuants (metrics)
  - sa and ind will work the same
  - phcr to convert FLKStock to FLQuants
  - hcr only needs metrics, not full FLStock (?)
- 
