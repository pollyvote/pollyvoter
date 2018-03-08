# this file collect all the initial helper functions of a pollyovte object
# naming convention is initial _ function type _ method name

#' initial pollyvote prediction
#' 
#' internal function which initializes the prediction function of method pollyvote.
#' This function aggregates the daat in two steps: 
#' In the first step all predictions of the same \code{source_types} are aggregated
#' daily. In the second step the aggregated \code{source_types} are aggregated,
#' resulting in one prediction per day and party. 
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object of which to get the prediction from.
#' @param time_int[\code{date}]
#'   the time interval for which the prediction should be made.
#' @param agg_fun [\code{character(1)}]\cr
#'   the name of the aggregation function to use, currently 'mean' and 'median' are supported
#'
#' @return data frame containing the results
#' 
#' @inheritParams fill_na
#' @export
initial_prediction_pollyvote = function(pv, time_int = NULL, agg_fun = "mean", na_handle = "last", ...) {
  
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
    get_data(time_int) %>% 
    fill_na(na_handle = na_handle, pv = pv, time_int = time_int) %>%
    group_by(date, source_type, party) %>% 
    summarize(percent = fun(percent, na.rm = TRUE)) %>%
    group_by(date, party) %>%
    summarize(percent = fun(percent, na.rm = TRUE)) 
}


initial_region_prediction_pollyvote = function(pv, time_int = NULL, agg_fun = "mean", na_handle = "last", 
                                                region_method = "wta") {
  
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
  
  region_weights = get_region_weights(pv) # Needs to be implemented!
  
  pv %>%
    get_data(time_int) %>% 
    #fill_na(na_handle = na_handle, pv = pv, time_int = time_int) %>%    # Problem !
    group_by(region, date, source_type, party) %>%
    summarize(percent = fun(percent, na.rm = TRUE)) %>%
    group_by(region, date, party) %>%
    summarize(percent = fun(percent, na.rm = TRUE)) %>%
    group_by(region, date) %>%
    handle_region_method(region_method) %>%
    left_join(region_weights, by = "region") %>%
    mutate(electoral_result = electoral_result * weight) %>%
    group_by(date, party) %>%
    summarize(electoral_result = sum(electoral_result, na.rm = TRUE))  %>%
    mutate(percent = electoral_result / sum(region_weights$weight))
}


handle_region_method = function(data, region_method) {
  
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
