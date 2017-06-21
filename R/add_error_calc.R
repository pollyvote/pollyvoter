#' add an error calculation (to a pollyvote object)
#' 
#' Adds a prediction, so far only implemented for pollyvote objects.
#'
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object to add the data to.
#' @param method [\code{character(1)}]\cr
#'   name of the error calculation.
#' @param fun [\code{function(pv)}]\cr
#' @param ... additional arguments
#' 
#' @return The pollyvote object with added prediction.
#'
#' @export
add_error_calc = function(pv, method, fun, ...) {
  UseMethod("add_error_calc")
}

#' add data to a pollyvote object
#' 
#' Adds data to a pollyvote opbject.
#'
#' @inheritParams add_error_calc
#' 
#' @return The pollyvote object with added prediction
#'
#' @export
add_error_calc.pollyvote = function(pv, method = "TODO", fun = function(pv){stop("TODO")}, ...) {
  # TODO checks on fun
  pv$error_calc[[method]] = fun
  return(pv)
}