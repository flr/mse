# data.R - Documentation for the mse package datasets
# mse/R/data.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

#' Example set of performance statistics
#'
#' A set of performance statistics is provided, coded in the format that
#' `performance()`requires. The statistics included in this list are as
#' follows:
#'
#' - SB0: Mean spawner biomass relative to unfished.
#' - SBMSY: Mean spawnwer biomass relative to SBMSY.
#' - Ftarget: Mean fishing mortality relative to target.
#' - FMSY: Mean fishing mortality relative to FMSY.
#' - green: Probability of being in Kobe green quadrant.
#' - red: Probability of being in Kobe red quadrant.
#' - PSBMSY: Probability of SB greater or equal to SBMSY.
#' - PBlim: Probability that spawner biomass is above Blim.
#' - risk1: ICES Risk 1, mean probability that spawner biomass is below Blim.
#' - risk2: ICES Risk 2, probability that spawner biomass is above Blim once.
#' - risk3: ICES Risk 3, max probability that spawner biomass is above Blim.
#' - C: Mean catch over years.
#' - VarC: Catch variability.
#' - PC0: Probability of fishery shutdown.
#'
#' Each indicator is itself a list object, with three elements, the first two
#' of them compulsory:
#'
#' - An unnamed element of class *formula*, e.g. `yearMeans(SB/SB0)`.
#' - name: A short name to be output on tables and plots, of class character,
#' e.g. "SB/SB[0]".
#' - desc: A longer description of the indicator, of class character, e.g. "Mean
#' spawner biomass relative to unfished"
#'
#' @docType data
#' @keywords datasets
#' @format An object of class list.
#' @name statistics
#' @rdname statistics
NULL

#' Kobe statistics
#' 
#' Aliquam sagittis feugiat felis eget consequat. Praesent eleifend dolor massa, 
#' vitae faucibus justo lacinia a. Cras sed erat et magna pharetra bibendum quis in 
#' mi. Sed sodales mollis arcu, sit amet venenatis lorem fringilla vel. Vivamus vitae 
#' ipsum sem. Donec malesuada purus at libero bibendum accumsan. Donec ipsum sapien, 
#' feugiat blandit arcu in, dapibus dictum felis. 
#' 
#' @docType data
#' @keywords datasets
#' @format An object of class list.
#' @name kobestatistics
#' @rdname kobestatistics
NULL

#' FLom object for North sea plaice
#' 
#' Aliquam sagittis feugiat felis eget consequat. Praesent eleifend dolor massa, 
#' vitae faucibus justo lacinia a. Cras sed erat et magna pharetra bibendum quis in 
#' mi. Sed sodales mollis arcu, sit amet venenatis lorem fringilla vel. Vivamus vitae 
#' ipsum sem. Donec malesuada purus at libero bibendum accumsan. Donec ipsum sapien, 
#' feugiat blandit arcu in, dapibus dictum felis. 
#' 
#' @docType data
#' @keywords datasets
#' @format An object of class *FLom*.
#' @name p4om
#' @rdname p4om
NULL

