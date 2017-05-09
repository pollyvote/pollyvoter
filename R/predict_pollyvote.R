#' predict a pollyvote object
#' 
#' predict a pollyvote opbject.
#'
#' @param object [\code{pollyvote(1)}] \cr
#'   pollyvote object to predict from.
#' @inheritParams add_prediction
#' 
#' @return the prediction
#' @family predict
#'
#' @export
predict.pollyvote = function(object, name, ...) {
  assert_class(object, "pollyvote")
  return(object$predictions[[name]](object))
}