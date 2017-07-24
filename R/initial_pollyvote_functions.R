# this file collect all the initial helper functions of a pollyovte object
# naming convention is initial _ function type _ method name

#' initial pollyvote prediction
#' 
#' internal function which initial prediction function of method pollyvote.
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
#' internal function which initial prediction function of method aggregated 
#' source type.
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



#' initial pollyvote error calculation
#' 
#' internal function which intial function for error calculation.
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction from.
#' @param prediction [\code{character(1)}]\cr
#'   the name of the prediction function 
#' @param election [\code{character(1)}]\cr
#'   the name of the prediction function
#' @param ci [\code{logical(1)}]\cr
#'   whether confidence interval should be calculated
#' @param alpha [\code{numeric(1)}]\cr
#'   significance level
#' @param no_days [\code{character(1)}]\cr
#'  
#'      
#' @return data frame containing the results
#' 
#' @inheritParams fill_na
#' @export
initial_error_calc_prediction_election = function(pv, prediction = "pollyvote", election, 
                                                  ci = FALSE, alpha = 0.05, no_days = Inf, ... ) {
  # extract election result
  if (length(pv$election_result) == 0)
    stop("pv does not contain any election results. Use add_election_result() to add the results of an election.")
  if (missing(election)) 
    election = names(pv$election_result)[1]
  result = get_election_result(pv, election)
  
  # extract predicted data
  pred_data = predict(pv, method = prediction, ...) %>%
    limit_days(no_days = no_days,
               election_data = result, ...)
  # bring the prediction and the result together
  joined = left_join(x = pred_data, y = result, by = "party") %>%
    #rename(percent = percent.x, percent.true = percent.y)
    ungroup %>%
    rename(percent = percent.x, percent.true = percent.y,
           date = date.x, election_date = date.y)
  error_dat = mutate(joined, error = abs(percent - percent.true))
  if(!ci) {
    return(error_dat)
  } else {
    ec_mean_error = error_dat %>% 
      group_by(party) %>%
      summarize(mean_error = mean(error))
    ec_ci = left_join(error_dat, ec_mean_error, by = "party") %>%
      mutate(ci_lower = percent - qnorm(1 - alpha / 2) * mean_error,
             ci_upper = percent + qnorm(1 - alpha / 2) * mean_error)
    return(ec_ci)
  }
  
}
