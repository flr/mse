#' @title S4 class \code{FLiem}
#'
#' @description The \code{FLiem} class stores the information relative to the implementation error model of the MSE.
#'
#' @section Slots:
#' \describe{
#'    \item{method}{\code{character} with the name of the method to be run. Note a function of method must exist in the environment with the same name.}
#'    \item{args}{\code{list} with arguments to be passed to the function defined in \code{method}}
#' @template Accessors
#' @template Constructors
#' @docType class
#' @name FLoem-class
#' @rdname FLoem-class
#' @aliases FLoem-class
#' @examples

FLiem <- setClass("FLiem", contains = "mseCtrl")


