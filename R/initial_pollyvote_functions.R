# this file collect all the initial heler functions of a pollyovte object
# naming convention is initial _ function type _ method name

#' initial pollyvote prediction
#' 
#' TODO
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction from.
#' @param agg_fun [\code{character(1)}]\cr
#'   the name of the aggregation function to use, currently 'mean' and 'median' are supported
#'
#' @return data frame containing the results
#' 
#' @inheritParams fill_na
#' @export
initial_prediction_pollyvote = function(pv, agg_fun = "mean", na_handle = "last", ...) {
  # TODO create limit_days argument either usign number of days to election or fixed date
  #  create a function that works like fill_na just for limit_days
  
  # input checking
  assert_class(pv, "pollyvote")
  # evaluate string input
  assert_choice(agg_fun, c("mean", "median"))
  # evaluate string input
  fun = switch(agg_fun,
               mean = mean,
               median = median)
  assert_choice(na_handle, c("last", "omit", "mean_within", "mean_across"))
  if(length(get_perm_source_types(pv)) != 0)
    lapply(which_source_type, assert_choice, get_perm_source_types(pv))
  
  pv %>%
    get_data %>% 
    fill_na(na_handle = na_handle, pv = pv) %>%
    group_by(date, source_type, party) %>%
    summarize(percent = fun(percent, na.rm = TRUE)) %>%
    group_by(date, party) %>%
    summarize(percent = fun(percent, na.rm = TRUE)) 
}


#' initial aggregated source type prediction
#' 
#' TODO
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction from.
#' @param which_source_type [\code{character(1)}]\cr
#'   the name of the source to use for aggregation 
#' @param agg_fun [\code{character(1)}]\cr
#'   the name of the aggregation function to use, currently 'mean' and 'median' are supported
#'
#' @return data frame containing the results
#' 
#' @inheritParams fill_na
#' @export
initial_prediction_aggr_source_type = function(pv, which_source_type, 
                agg_fun = "mean", 
                na_handle = "last", ...) {
  # input checking
  assert_class(pv, "pollyvote")
  assert_choice(agg_fun, c("mean", "median"))
  # evaluate string input
  fun = switch(agg_fun,
               mean = mean,
               median = median)
  assert_choice(na_handle, c("last", "omit", "mean_within", "mean_across"))
  if(length(get_perm_source_types(pv)) != 0)
    lapply(which_source_type, assert_choice, get_perm_source_types(pv))
  
  pv %>% 
    get_data  %>%
    filter(source_type %in% which_source_type) %>%
    fill_na(na_handle = na_handle, pv = pv) %>%
    group_by(date, source_type, party) %>% 
    summarize(percent = fun(percent, na.rm = TRUE))
}

