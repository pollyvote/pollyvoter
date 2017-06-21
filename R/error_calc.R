#' calculate the prediction error of a pollyvote prediction.
#' 
#' calculate the prediction error of a pollyvote prediction.
#'
#' @param object [\code{pollyvote(1)}] \cr
#'   pollyvote object to get the error calculation from.
#' @param method [\code{character(1)}] \cr
#'   method name of the error calculation function of \code{object} to use.
#' @param ... additional arguments to the error calculation function.
#' 
#' @return a data frame containing the result of the error calculation
#' @family predict
#'
#' @export
error_calc = function(object, method, ...) {
  UseMethod("error_calc")
}

#' calculate the prediction error of a pollyvote prediction.
#' 
#' calculate the prediction error of a pollyvote prediction.
#'
#' @inheritParams error_calc
#' 
#' @return the prediction
#' @family predict
#'
#' @export
error_calc.pollyvote = function(object, method, ...) {
  assert_class(object, "pollyvote")
  return(object$error_calc[[method]](object, ...))
}