#' calculate the prediction error of a pollyvote prediction.
#' 
#' calculate the prediction error of a pollyvote prediction.
#'
#' @param pv [\code{pollyvote(1)}] \cr
#'   pollyvote object to predict from.
#' @param name [\code{character(1)}] \cr
#'   name of the error calcluclation function on \code{pr} to use.
#' @param ... currently unused.
#' 
#' @return the prediction
#' @family predict
#'
#' @export
error_calc = function(pv, name, ...) {
  assert_class(pv, "pollyvote")
  return(pv$error_calc[[name]](pv))
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
error_calc.pollyvote = function(pv, name, ...) {
  assert_class(pv, "pollyvote")
  return(pv$error_calc[[name]](pv))
}