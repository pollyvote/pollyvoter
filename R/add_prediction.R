#' add a prediciton (to a pollyvote object)
#' 
#' Adds a prediction, so far only implemented for pollyvote objects.
#'
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object to add the data to.
#' @param name[\code{character(1)}]\cr
#'   name of the prediction.
#' @param fun [\code{function(pv)}]\cr
#' @param ... additional arguments
#' 
#' @return The pollyvote object with added prediction.
#'
#' @export
add_prediction = function(pv, name, fun = function(x){x}, ...) {
  UseMethod("add_prediction")
}

#' add data to a pollyvote object
#' 
#' Adds data to a pollyvote opbject.
#'
#' @inheritParams add_prediction
#' 
#' @return The pollyvote object with added prediction
#'
#' @export
add_prediction.pollyvote = function(pv, name = "no_aggregation", fun = function(pv){get_data(pv)}, ...) {
  pv$predictions[[name]] = fun
  return(pv)
}

