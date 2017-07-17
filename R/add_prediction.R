#' add a prediciton (to a pollyvote object)
#' 
#' Adds a prediction, so far only implemented for pollyvote objects.
#'
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object to add the data to.
#' @param method [\code{character(1)}]\cr
#'   method of the prediction. This method name will be used to call the prediction in \code{\link{predict.pollyvote}}.
#' @param fun [\code{function(pv)}]\cr
#' @param ... additional arguments
#' 
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' pv = add_prediction(pv, "poll", function(pv) {
#'   pv %>% 
#'     get_data %>% 
#'     filter(source_type %in% c("poll")) %>%
#'     group_by(date, source_type, party) %>% 
#'     summarize(percent = mean(percent, na.rm = TRUE))
#' })
#' 
#' @return The pollyvote object with added prediction.
#'
#' @export
add_prediction = function(pv, method, fun = function(pv){get_data(pv)}, ...) {
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
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' pv = add_prediction(pv, "poll", function(pv) {
#'   pv %>% 
#'     get_data %>% 
#'     filter(source_type %in% c("poll")) %>%
#'     group_by(date, source_type, party) %>% 
#'     summarize(percent = mean(percent, na.rm = TRUE))
#' })
#'
#' @export
add_prediction.pollyvote = function(pv, method = "no_aggregation", fun = function(pv){get_data(pv)}, ...) {
  # TODO checks on fun
  pv$predictions[[method]] = fun
  return(pv)
}

#' add an aggregation function to a pollyvote object
#' 
#' Adds an aggregation function to a pollyvote object. An aggregation function is
#' a special type of a prediction function.
#'
#' @inheritParams add_prediction
#' @param which_source_type [\code{character(n)}]\cr
#'   character vector of arbitrary length containing the \code{source_type}s 
#'   over which to aggregate.
#' @param agg_fun [\code{character(1)}]\cr
#'   string indicating which aggregation function to use. Currently implemented 
#'   are 'mean' and 'median'.
#' @param na_handle [\code{character(1)}]\cr
#'   string indicating which aggregation function to use. Currently implemented 
#'   are 'na.rm', indicating that missing observations will be ignored and all
#'   other functions will be aggregated and TODO.
#' @importFrom stats median
#' @return The pollyvote object with added prediction
#' @family add_aggr
#'
#' @export
add_aggr_source_type = function(pv, method, which_source_type, agg_fun = "mean", 
                                na_handle = "last", ...) {
  # input checking
  assert_class(pv, "pollyvote")
  assert_character(method)
  if(length(get_perm_source_types(pv)) != 0)
    lapply(which_source_type, assert_choice, get_perm_source_types(pv))
  assert_choice(agg_fun, c("mean", "median"))
  assert_choice(na_handle, c("last", "omit", "mean_within", "mean_across"))
  
  # evaluate string input
  fun = switch(agg_fun,
               mean = mean,
               median = median)
  # na.rm = ifelse(na_handle == "na.rm", TRUE, FALSE)
  
  # call add_prediction with the suitable aggregation function
  # add_prediction(pv, method, function(pv) {
  #   pv %>% 
  #     get_data %>% 
  #     filter(source_type %in% which_source_type) %>%
  #     group_by(date, source_type, party) %>% 
  #     # TODO NA handling here?
  #     summarize(percent = fun(percent, na.rm = na.rm))
  # })
  add_prediction(pv, method, function(pv) {
    pv %>% 
      get_data %>%
      filter(source_type %in% which_source_type) %>%
      fill_na(na_handle = na_handle, pv = pv, ...) %>%
      group_by(date, source_type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  
}
