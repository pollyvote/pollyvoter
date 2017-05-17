#' add a prediciton (to a pollyvote object)
#' 
#' Adds a prediction, so far only implemented for pollyvote objects.
#'
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object to add the data to.
#' @param name [\code{character(1)}]\cr
#'   name of the prediction.
#' @param fun [\code{function(pv)}]\cr
#' @param ... additional arguments
#' 
#' @return The pollyvote object with added prediction.
#'
#' @export
add_prediction = function(pv, name, fun = function(pv){get_data(pv)}, ...) {
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
  # TODO checks on fun
  pv$predictions[[name]] = fun
  return(pv)
}

#' add an aggregation function to a pollyvote object
#' 
#' Adds an aggregation function to a pollyvote object. An aggregation function is
#' a special type of a prediction function.
#'
#' @inheritParams add_prediction
#' @param which.source.type [\code{character(n)}]\cr
#'   character vector of arbitrary length containing the \code{source.type}s 
#'   over which to aggregate.
#' @param agg_fun [\code{character(1)}]\cr
#'   string indicating which aggregation function to use. Currently implemented 
#'   are 'mean' and 'median'.
#' @param na.handle [\code{character(1)}]\cr
#'   string indicating which aggregation function to use. Currently implemented 
#'   are 'na.rm', indicating that missing observations will be ignored and all
#'   other functions will be aggregated and TODO.
#' @importFrom stats median
#' @return The pollyvote object with added prediction
#' @family add_aggr
#'
#' @export
add_aggr_source.type = function(pv, name, which.source.type, agg_fun = "mean", 
                                na.handle = "na.rm", ...) {
  # input checking
  assert_class(pv, "pollyvote")
  assert_character(name)
  # TODO replace pv$perm_source.types with getter function
  if(length(pv$perm_source.types) != 0)
    lapply(which.source.type, assert_choice, pv$perm_source.types)
  assert_choice(agg_fun, c("mean", "median"))
  assert_choice(na.handle, "na.rm")
  
  # evaluate string input
  fun = switch(agg_fun,
               mean = mean,
               median = median)
  na.rm = ifelse(na.handle == "na.rm", TRUE, FALSE)
  
  # call add_prediction with the suitable aggregation function
  add_prediction(pv, name, function(pv) {
    pv %>% 
      get_data %>% 
      filter(source.type %in% which.source.type) %>%
      group_by(date, source.type, party) %>% 
      # TODO NA handling here?
      summarize(percent = fun(percent, na.rm = na.rm))
  })
}

