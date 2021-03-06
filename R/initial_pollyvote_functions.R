# this file collect all the initial helper functions of a pollyovte object
# naming convention is initial _ function type _ method name

#' initial pollyvote prediction
#' 
#' internal function which initializes the prediction function of method pollyvote.
#' This function aggregates the daat in two steps: 
#' In the first step all predictions from sources of the same \code{source_types} are aggregated
#' daily. In the second step the aggregated \code{source_types} are aggregated,
#' resulting in one prediction per day and party. 
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction data from.
#' @param time_int[\code{date}]
#'   the time interval for which the prediction.
#' @param agg_fun [\code{character(1)}]\cr
#'   the name of the aggregation function to use, currently 'mean' and 'median' are supported
#' @param na_handle [\code{character(1)}]\cr 
#'   specifies how NA values are handled in the data.
#'
#' @return data frame containing predictions for a parties in the days before the election
#' 
#' @inheritParams fill_na
#' @export
initial_prediction_pollyvote = function(pv, time_int = NULL, agg_fun = "mean", na_handle = "last", ...) {
  
  validate_common_prediction_params(pv = pv, agg_fun = agg_fun, na_handle = na_handle)
  
  # evaluate string input
  fun = switch(agg_fun,
               mean = mean,
               median = median)
  
  pv %>%
    get_data(time_int) %>% 
    fill_na(na_handle = na_handle, pv = pv, time_int = time_int) %>%
    group_by(date, source_type, party) %>% 
    summarize(percent = fun(percent, na.rm = TRUE)) %>%
    group_by(date, party) %>%
    summarize(percent = fun(percent, na.rm = TRUE)) 
}

#' Calculates prediction for each party on each day in each region from the data in the pollyvote container.
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction data from.
#' @param time_int[\code{date}]
#'   the time interval for the prediction.
#' @param agg_fun [\code{character(1)}]\cr
#'   the name of the aggregation function to use, currently 'mean' and 'median' are supported
#' @param na_handle [\code{character(1)}]\cr 
#'   specifies how NA values are handled in the data.
#' @param region [\code{character(1)}] \cr
#'   The region in which predictions are made. If the region is not existing in the pollyvote container, error is thrown.
#'   If [\code{NULL}], then the predictions are calculated in each region. 
#'   
#' @return dataframe of predictions for a party on each day in each region.
#' 
#' @export
initial_region_prediction_pollyvote = function(pv, time_int = NULL, agg_fun = "mean", na_handle = "last", region = NULL) {
  
  validate_common_prediction_params(pv = pv, agg_fun = agg_fun, na_handle = na_handle)
  
  data = pv %>%
    get_data(time_int)
  
  if (!is.null(region)) {
    # Should we let the user to use this prediction function without adding regions to the pollyvote container ?
    region_weights = get_region_weights(pv)
    assert_choice(region, region_weights$region)
    
    data = data[data$region == region, ]
    if (nrow(data) == 0) {
      stop(paste("No observations in the data with region =", region))
    }
  }
  
  fun = switch(agg_fun,
               mean = mean,
               median = median)
  
   data %>% 
    fill_na(na_handle = na_handle, pv = pv, time_int = time_int) %>%
    group_by(region, date, source_type, party) %>%
    summarize(percent = fun(percent, na.rm = TRUE)) %>%
    group_by(region, date, party) %>%
    summarize(percent = fun(percent, na.rm = TRUE))
}

#' First calculates predictions on level of each region by using \code{initial_region_prediction_pollyvote} function
#' and afterwards aggregates predictions over the regions in order to obtain final single prediction for a party on given day.
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction data from.
#' @param time_int[\code{date}]
#'   the time interval for the prediction.
#' @param agg_fun [\code{character(1)}]\cr
#'   the name of the aggregation function to use, currently 'mean' and 'median' are supported
#' @param na_handle [\code{character(1)}]\cr 
#'   specifies how NA values are handled in the data.
#' @param region_method method of aggregation of party scores over different regions.
#'   See \code{handle_region_method} function for implementation and meaning of the parameteres.
#' 
#' @return dataframe containing aggregated predictions for the parties aggregated over regions.
#' 
#' @export
#'
initial_region_aggregation_pollyvote = function(pv, time_int = NULL, agg_fun = "mean", na_handle = "last",
                                                region_method = c("wta", "vs")) {
  validate_common_prediction_params(pv = pv, agg_fun = agg_fun, na_handle = na_handle)
  
  region_method = match.arg(region_method)
  
  fun = switch(agg_fun,
               mean = mean,
               median = median)
  
  region_weights = get_region_weights(pv)
  
  initial_region_prediction_pollyvote(pv, time_int, agg_fun, na_handle) %>%
    handle_region_method(region_method) %>%
    left_join(region_weights, by = "region") %>%
    mutate(electoral_result = electoral_result * weight) %>% #weights in case of 'wta' should be positive integers and in case of 'vs' numbers between 0 and 1 ? 
    group_by(date, party) %>%
    summarize(electoral_result = sum(electoral_result, na.rm = TRUE))  %>%
    group_by(date) %>%
    mutate(percent = 100 * electoral_result / sum(electoral_result))
}

#' Adds new column named 'electoral_result' in the input [\code{data}] parameter depending on the value of [\code{region_method}] parameter.
#' 
#' @param data [\code{dataframe}]\cr
#'   The predictions dataframe
#' @param region_method [\code{character(1)}]
#'  Values:
#'   \itemize{
#'    \item [\code{character(1)}] wta: Winner takes it all method - 1 is assigned to the party which has most points in a region for a given day.
#'    \item [\code{character(1)}] vs:  Voteshares method - percent for the parties remain unchanged.
#'    }
#' 
#' @return dataframe with 'electoral_result' column which a result of applying region aggregation method.
#' @export
handle_region_method = function(data, region_method) {
  assert_choice(region_method, c("wta", "vs"))
  
  data = data %>% group_by(region, date)
  if (region_method == 'wta') {
    return(data %>% mutate(electoral_result = ifelse(percent == max(percent, na.rm = TRUE), 1, 0)))
  } else {
    data %>% mutate(electoral_result = percent)
  }
}


#' initial aggregated source type prediction
#' 
#' internal function which initializes a prediction function of method
#' \code{'aggr_source_type'}. This functions selects one source type and aggregates
#' the different sources daily per party.
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
  
  validate_common_prediction_params(pv = pv, agg_fun = agg_fun, na_handle = na_handle)
  
  # evaluate string input
  fun = switch(agg_fun,
               mean = mean,
               median = median)
  
  if(length(get_perm_source_types(pv)) != 0)
    lapply(which_source_type, assert_choice, get_perm_source_types(pv))
  
  pv %>% 
    get_data  %>%
    filter(source_type %in% which_source_type) %>%
    fill_na(na_handle = na_handle, pv = pv) %>%
    group_by(date, source_type, party) %>% 
    summarize(percent = fun(percent, na.rm = TRUE))
}

#' Validates common input parameters to the prediction functions.
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction data from.
#' @param time_int[\code{date}]
#'   the time interval for the prediction.
#' @param agg_fun [\code{character(1)}]\cr
#'   the name of the aggregation function to use, currently 'mean' and 'median' are supported
#' @param na_handle [\code{character(1)}]\cr 
#'   specifies how NA values are handled in the data.
#'   
#'   
validate_common_prediction_params = function(pv, agg_fun, na_handle) {
  assert_class(pv, "pollyvote")
  assert_choice(agg_fun, c("mean", "median"))
  assert_choice(na_handle, c("last", "omit", "mean_within", "mean_across"))
}


#' initial pollyvote error calculation
#' 
#' internal function which intializes a function for error calculation of method
#' \code{'prediction_election'}. This means, that one election result of the pollyvote
#' container is compared with the result of a prediction function of the same
#' pollyvote container.
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction from.
#' @param prediction [\code{character(1)}]\cr
#'   the name of the prediction function 
#' @param target_election_year [\code{integer(1)}]\cr
#'   the election year for which confidence intervals are calculated.
#'   If \code{target_election_year} is \code{NULL}, CI are calculated for the last known election.
#' @param ci [\code{logical(1)}]\cr
#'   whether confidence interval should be calculated
#' @param alpha [\code{numeric(1)}]\cr
#'   significance level
#' @param no_days [\code{character(1)}]\cr
#' @param days_average
#' length of moving average 
#'      
#' @return data frame containing the results
#' 
#' @inheritParams fill_na
#' @export
initial_error_calc_prediction_election = function(pv, prediction = "pollyvote", target_election_year = NULL, ci = FALSE, alpha = 0.05,
                                                  no_days = Inf, moving_average = TRUE, days_average = 7,
                                                  ... ) {
  assert_class(pv, "pollyvote")
  # Once get_election_Result is fixed, get the election result from there, not directly from pv object.
  if (nrow(pv$election_result) == 0)
    stop("pv does not contain any election results. Use add_election_result() to add the results of an election.")
  
  # extract predicted data
  all_election_dates = sort(unique(get_election_result(pv)$date))
  if(min(get_data(pv)$date) < all_election_dates[1]){
    dummy_date = all_election_dates[1] - as.difftime(365, units = "days")
    all_election_dates = c(dummy_date, all_election_dates)
  }
  
  #validation of target_election_year param
  target_election_date = get_election_date_from_election_year(pv, target_election_year)
  
  pred_election_dates = all_election_dates[all_election_dates <= target_election_date]
  predictions_vs_actual = data.frame()
  for (i_date in 2:length(pred_election_dates)) {
    
    election_date <- pred_election_dates[i_date]
    election_result = subset(pv$election_result, date == election_date)
    time_int_i = c(pred_election_dates[i_date - 1], election_date)
    pred_data = predict(pv, time_int = time_int_i, method = prediction, ...) %>%
      limit_days(no_days = no_days, election_data = election_result, ...) %>%
      mutate(days_to_election = round(as.numeric(difftime(election_date, date, units="days"))))
    
    # bring the prediction and the result together
    joined = left_join(x = pred_data, y = election_result, by = "party") %>%
      ungroup %>%
      rename(percent = percent.x, percent.true = percent.y,
             date = date.x, election_date = date.y)
    
    predictions_vs_actual = rbind(predictions_vs_actual, joined)
  }
  
  error_dat = mutate(predictions_vs_actual, error = abs(percent - percent.true))
  if(!ci) {
    return(error_dat)
  } else {
    ec_mean_error = error_dat %>% 
      group_by(days_to_election, party) %>%
      summarize(mean_error = mean(error))
    
    error_dat = subset(error_dat, election_date == target_election_date)
    ec_ci = left_join(error_dat, ec_mean_error, by = c("days_to_election", "party")) %>%
      mutate(ci_lower = percent - qnorm(1 - alpha) * mean_error * 1.25,
             ci_upper = percent + qnorm(1 - alpha) * mean_error * 1.25)
    
    if(moving_average){
      ec_ci = moving_average_ci(ec_ci, days_average = days_average)
    }
    return(ec_ci)
  }
  
}

#' moving average
#' 
#' Calculate moving average for confidence intervals. 
#' 
#' @param data [\code{data.frame}]\cr
#'   data frame from ...
#'
#' @return data frame containing the results
#' 
#' @export
moving_average_ci = function(data, days_average = 7){
  
  # check if even number
  if (days_average %% 2 == 0){
    days_average = 7
    warning("days_average set to 7!")
  }
  
  parties = as.character(unique(data$party))
  
  # lower ci
  data_lower = data[, c("date", "party", "ci_lower")]
  data_lower = spread(data_lower, party, ci_lower)
  len = nrow(data_lower)
  date_vec = data_lower$date
  
  data_lower = as.data.frame(rollapply(data_lower[,-1], days_average, 
                                       mean, na.rm = TRUE,
                                        by.column = TRUE))
  data_lower$date = date_vec[ceiling(days_average/2):(len - floor(days_average/2))]
  data_lower = gather(data_lower, party, ci_lower, parties)
  
  
  # upper ci
  data_upper = data[, c("date", "party", "ci_upper")]
  data_upper <- spread(data_upper, party, ci_upper)
  
  data_upper = as.data.frame(rollapply(data_upper[,-1], days_average,
                                       mean, na.rm = TRUE, by.column = TRUE))
  data_upper$date = date_vec[ceiling(days_average/2):(len - floor(days_average/2))]
  data_upper = gather(data_upper, party, ci_upper, parties)
  
  data = subset(data, select = -c(ci_lower, ci_upper))
  data = merge(data, data_lower, by = c("date", "party"))
  data = merge(data, data_upper, by = c("date", "party"))
  
  return(data)
}
