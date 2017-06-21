#' plot a pollyvote object
#' 
#' plots a prediction of a pollyvote object.
#'
#' @param x [\code{pollyvote(1)}] \cr
#'   pollyvote object to predict from.
#' @param prediction_method [\code{character(1)}] \cr
#'   method of the prediction to be plotted.
#' @param error_calc_method [\code{character(1)}] \cr
#'   method of the error calculation to be plotted. Note that only one of prediction_method 
#'   and error_calc_method can be specified.
#' @param ... additional arguments to the predict or error_calc_method function
#' 
#' @return a ggplot object that can be further modified.
#' @import ggplot2
#' @family plot
#'
#' @export
plot.pollyvote = function(x, prediction_method = NULL, error_calc_method = NULL, ...) {
  assert_class(x, "pollyvote")
  # TODO change x$predictions/error_calc with getter function
  if(!is.null(prediction_method)) assert_choice(prediction_method, names(x$predictions))
  if(!is.null(error_calc_method)) assert_choice(error_calc_method, names(x$error_calc))
  if(!is.null(prediction_method) & !is.null(error_calc_method)) 
    error("Please specify either the 'prediction_method' or the 'error_calc_method' argument.")
  if(is.null(prediction_method) & is.null(error_calc_method)) 
    error("Please specify one of 'prediction_method' or 'error_calc_method'.")
  
  if(!is.null(prediction_method)){
    pred_data = predict.pollyvote(x, method = prediction_method, ...)
    p = ggplot(pred_data, aes(x = date, y = percent, color = party, group = party)) +
      geom_line()
  } else {
    pred_data = error_calc(x, method = error_calc_method, ...)
    p = ggplot(pred_data, aes(x = date, y = percent, color = party, group = party)) +
      geom_line()
  }
  return(p)
}