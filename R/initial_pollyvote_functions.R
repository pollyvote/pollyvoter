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
                                                  ci = FALSE, alpha = 0.05, no_days = Inf, 
                                                  moving_average = TRUE, ... ) {
  # extract election result
  if (length(pv$election_result) == 0)
    stop("pv does not contain any election results. Use add_election_result() to add the results of an election.")
  if (missing(election)) 
    election = names(pv$election_result)[1]
  result = get_election_result(pv, election)
  election_date = result[["date"]][[1]]
  
  # extract predicted data
  pred_data = predict(pv, method = prediction, ...) %>%
    limit_days(no_days = no_days,election_data = result, ...) %>%
    mutate(days_to_election = as.numeric(difftime(election_date, date, units="days")))
  # bring the prediction and the result together
  joined = left_join(x = pred_data, y = result, by = "party") %>%
    ungroup %>%
    rename(percent = percent.x, percent.true = percent.y,
           date = date.x, election_date = date.y)
  error_dat = mutate(joined, error = abs(percent - percent.true))
  if(!ci) {
    return(error_dat)
  } else {
    ec_mean_error = error_dat %>% 
      group_by(days_to_election, party) %>%
      summarize(mean_error = mean(error))
    ec_ci = left_join(error_dat, ec_mean_error, by = c("days_to_election", "party")) %>%
      mutate(ci_lower = percent - qnorm(1 - alpha) * mean_error * 1.25,
             ci_upper = percent + qnorm(1 - alpha) * mean_error * 1.25)
    
    if(moving_average){
      ec_ci = moving_average_ci(ec_ci)
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
moving_average_ci = function(data){
  
  parties = unique(data$party)
  
  # lower ci
  data_lower = data[, c("date", "party", "ci_lower")]
  data_lower <- spread(data_lower, party, ci_lower)
  len = nrow(data_lower)
  date_vec = data_lower$date
  
  data_lower = as.data.frame(rollapply(data_lower[,-1], 7, mean, na.rm = TRUE,
                                        by.column = TRUE))
  data_lower$date = date_vec[4:(len - 3)]
  data_lower = gather(data_lower, party, ci_lower, parties)
  
  
  # upper ci
  data_upper = data[, c("date", "party", "ci_upper")]
  data_upper <- spread(data_upper, party, ci_upper)
  
  data_upper = as.data.frame(rollapply(data_upper[,-1], 7, mean, na.rm = TRUE,
                                        by.column = TRUE))
  data_upper$date = date_vec[4:(len - 3)]
  data_upper = gather(data_upper, party, ci_upper, parties)
  
  data = subset(data, select = -c(ci_lower, ci_upper))
  data = merge(data, data_lower, by = c("date", "party"))
  data = merge(data, data_upper, by = c("date", "party"))
  
  return(data)
}
