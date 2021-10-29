# generics.R - DESC
# /generics.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# debug, undebug {{{

#' Debugging mse modules
#'
#' Set and unset the debugging flag of a function inside the *method* slot of
#' a mseCtrl object.
#'
#' Modules in the mse control object contain the function to be called in the
#' *method* slot. To debug and check the behaviour of an individual function,
#' the *debug* method will start a browser session next time it is called.
#' Debugging functions requires the parallel flag to be set to FALSE, or that
#' no parallel backend is loaded.
#'
#' @param fun Module or control object to debug.
#' @param text Name of module in mpCtrl.
#' @param condition Unused.
#' @param signature Name of module in mpCtrl.
#'
#' @return Both functions invisibly return NULL
#'
#' @name debug-mse
#' @rdname debug-mse
#' @aliases debug-mse
#' @author Iago Mosqueira (WMR)
#' @seealso [`debug`]
#' @keywords methods

setGeneric("debug", useAsDefault = base::debug)
setGeneric("undebug", useAsDefault = base::undebug)

# TODO
# debugonce(fun, text = "", condition = NULL, signature = NULL)
# isdebugged(fun, signature = NULL)
# debuggingState(on = NULL)

# }}}
