#' predict a pollyvote object
#' 
#' predict a pollyvote opbject.
#'
#' @inheritParams add_prediction
#' 
#' @return the prediction
#'
#' @export
predict.pollyvote = function(pv, name, ...) {
  return(pv$predictions[[name]](pv))
}