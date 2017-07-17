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
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' pv = add_error_calc(pv, "poll_only", function(pv) {
#'   pred_data = predict(pv, "poll")
#'   result = get_election_result(pv, "BTW")
#'   joined = left_join(x = pred_data, y = result, by = "party") %>%
#'     rename(percent = percent.x, percent.true = percent.y)
#'   return(mutate(joined, error = abs(percent - percent.true)))
#' })
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
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' pv = add_error_calc(pv, "poll_only", function(pv) {
#'   pred_data = predict(pv, "poll")
#'   result = get_election_result(pv, "BTW")
#'   joined = left_join(x = pred_data, y = result, by = "party") %>%
#'     rename(percent = percent.x, percent.true = percent.y)
#'   return(mutate(joined, error = abs(percent - percent.true)))
#' })
#'
#' @export
add_error_calc.pollyvote = function(pv, method = "TODO", fun = function(pv){stop("TODO")}, ...) {
  # TODO checks on fun
  pv$error_calc[[method]] = fun
  return(pv)
}