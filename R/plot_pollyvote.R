#' plot a pollyvote object
#' 
#' plots a prdiction of a pollyvote opbject.
#'
#' @param x [\code{pollyvote(1)}] \cr
#'   pollyvote object to predict from.
#' @param prediction [\code{character(1)}] \cr
#'   name of the prediction to be plotted.
#' @param error_calc [\code{character(1)}] \cr
#'   name of the error calculation to be plotted. Note that only one of prediction 
#'   and error_calc can be specified.
#' @param ... additional arguments to the predict or error_calc function
#' 
#' @return a ggplot object that can be further modified.
#' @import ggplot2
#' @family plot
#'
#' @export
plot.pollyvote = function(x, prediction = NULL, error_calc = NULL, ...) {
  assert_class(x, "pollyvote")
  # TODO change x$predictions/error_calc with getter function
  if(!is.null(prediction)) assert_choice(prediction, names(x$predictions))
  if(!is.null(error_calc)) assert_choice(error_calc, names(x$error_calc))
  if(!is.null(prediction) & !is.null(error_calc)) 
    error("Please specify either the 'prediction' or the 'error_calc' argument.")
  if(is.null(prediction) & is.null(error_calc)) 
    error("Please specify one of 'prediction' or 'error_calc'.")
  
  if(!is.null(prediction)){
    pred_data = predict(x, name = prediction, ...)
    p = ggplot(pred_data) +
      geom_line(aes(x = date, y = percent, color = party, group = party))
  } else {
    pred_data = error_calc(x, name = error_calc, ...)
    p = ggplot(pred_data) +
      geom_line(aes(x = date, y = percent, color = party, group = party))
  }
  return(p)
}