## ---- pkgs, echo=FALSE, message=FALSE-----------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>",
  fig.align="center")
library(mse)

## ----FLom, echo=FALSE, out.width='50%', fig.cap="**FLom class**: Structure of the FLom class. Slot name followed by the slot class, in parenthesis."----
knitr::include_graphics('diagrams/FLom.png')

## ----FLmse, echo=FALSE, out.width='50%', fig.cap="**FLmse class**: Structure of the FLmse class. Slot name followed by the slot class, in parenthesis."----
knitr::include_graphics('diagrams/FLmse.png')

## ----mp, echo=FALSE, out.width='50%', fig.cap="Workflow of the mp() function."----
knitr::include_graphics('diagrams/mp.svg')

## ---- devtools, echo=TRUE, eval=FALSE-----------------------------------------
#  	library(devtools)
#  	install_github('flr/mse')

