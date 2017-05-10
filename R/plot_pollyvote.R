#' plot a pollyvote object
#' 
#' plots a prdiction of a pollyvote opbject.
#'
#' @param x [\code{pollyvote(1)}] \cr
#'   pollyvote object to predict from.
#' @param pred_name [\code{character(1)}] \cr
#'   name of the prediction to be plotted.
#' @param ... currently unused.
#' 
#' @return a ggplot object that can be further modified.
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
  
  # TODO implement method for error_calc
  pred_data = predict(pv, "poll")
  p = ggplot(pred_data) +
    geom_line(aes(x = date, y = percent, color = party, group = party))
  return(p)
}