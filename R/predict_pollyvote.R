#' predict a pollyvote object
#' 
#' predict a pollyvote opbject.
#'
#' @param object [\code{pollyvote(1)}] \cr
#'   pollyvote object to predict from.
#' @inheritParams add_prediction
#' 
#' @examples
#' pv = create_pollyvote(perm_countries = "D") 
#' data("polls_individual")
#' pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
#'               source_type = "poll", election = "BTW")
#' pred <- predict(pv, method = "pollyvote")               
#'               
#' 
#' @return a data frame containing the result of the prediction
#' @family predict
#'
#' @export
predict.pollyvote = function(object, method, ...) {
  assert_class(object, "pollyvote")
  return(object$predictions[[method]](object, ...))
}