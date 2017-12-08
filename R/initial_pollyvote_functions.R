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

#' Coalitions percentage prediction.
#'  
#' Provides prediction of percentages from different source types for specified coalitions. 
#' Coalitions percentages are predicted for each day in range [election_day - limit_days, election_day). 
#'  
#' @param pv [\code{pollyvote}]\cr 
#'   The pollyvote object of which to get the prediction from.
#' @param coalitions [\code{list}]\cr
#'   List of vectors representing coallitions. Coallitions must be specified with full and exact party names.
#' @param threshold [\code{numeric(1)}]\cr
#'   If positive, this parameter indicates the minimum voice component (greater or equal) 
#'   that a party has to reach in order to participate in a coalition.
#' @param threshold_handle [\code{character(1)}]\cr
#'   Specifies how to handle coalitions with parties that have less percentage than threshold.
#'   Options:
#'    o omit: default value. In this case, this coalition forfeits for the given date, instead, NA is entered.
#'    o ignore: In this case, the coalition will be made up of the remaining parties.                                                                          one).
#' @param prediction [\code{numeric(1)}]\cr
#'   Name of the prediction function.
#'   The Component definition deviates here somewhat from the other declarations to the Normally to come closer.
#'   Default value is the string specification of the root prediction, ie"Pollyvote", at a transversal (i.e. non-regional) level. Here can be handed over
#'    o character - that is, a single component name (e.g., "poll"); in this case will one column for the specified component
#'      and one column each (direct) Subcomponent returned
#'    o Vector with component name - gives exactly for the specified components (without automatic subcomponent addition) the values back
#'    o NULL - Wildcard that returns all components
#' @param election_year [\code{numeric(1)}]\cr
#'   The election year for which the coalitions are predicted.
#'   If not specified, the most recent election year is used.
#' @param permitted_parties [\code{character(1)}]\cr
#'   Allows the selection of specific parties.
#'   Options:
#'    o character - For specifying one party - DOES SPECIFYING ONE PARTY MAKES SENSE ?
#'    o Vector - For specifying multiple parties
#'    o NULL - For all parties
#' @param region [\code{character(1)}]\cr
#'   The region for which to calculate coalitions predictions.
#'   If the specified region is not defined, error is thrown.
#'   Options:
#'    o character - the Specified region.
#'    o NULL - If the region is not specified, then the result over all regions is returned.
#' @param limitdays [\code{numeric(1)}]\cr
#'   Limit in days before the election up to which the coalitions percentages are calculated.
#'   For example, specifying limitdays = 100 return coalitions percentages up to 100 days before the election.
#'   If negative number is supplied (default value), then data from all days is taken into account when calculating coalition percentages.
#' @param for.ggplot2 [\code{logical(1)}]\cr
#'   Return format of coalitions predictions.
#'   Options:
#'    o FALSE(default) - Returns data frame of columns (date | Days to election | Coalition_1_percentage | ... | Coaltion_n_percentage)
#'    o TRUE - Data frame with rows containing visualisation points
#' 
#' @return dataframe of columns (date | Days to election | Coalition_1_percentage | ... | Coaltion_n_percentage)
#'   or visualisation points of the coalitions prediction.
#' @export
initial_coalitions_pred = function(pv, coalitions, threshold = 0, threshold_handle = 'omit', prediction ='pollyvote',
                           election_year = NULL, permitted_parties = NULL, region = NULL, limitdays = -1, for.ggplot2 = FALSE ) {
  
  #' Checks whether coalitions are made of permitted parties.
  #' Permitted parties can be defined either in pv$permparties or in allowed_parties parameter.
  #' Therefore, the idea is first to collect all specified permitted parties in one vector.
  #' This vector will serve as a source for checking whether parties inside the coalitions have valid names.
  #' If there are no parties specified, then the check for valid party names is not performed.
  #' 
  #' @param input coalitions to the function.
  #' @param pv the pollyvote object.
  #' @param allowed_parties the input allowed_parties object.
  #' 
  #' 
  #' @return list of coalitions consisted only of permitted parties.
  get_valid_coalitions = function(coalitions, pv, permitted_parties) {
    assert_list(coalitions)
    
    all_permitted_parties = character(0)
    if (!is.null(pv$perm_parties)){
      all_permitted_parties = c(all_permitted_parties, pv$perm_parties)
    }
    if (!is.null(permitted_parties)){
      all_permitted_parties = c(all_permitted_parties, permitted_parties)
    }
    
    if (length(all_permitted_parties) == 0) {
      return(coalitions)
    }
    
    all_permitted_parties = unique(all_permitted_parties)
    
    are_with_permitted_parties = sapply(coalitions, function(coalition){
      valid_party_names = sapply(coalition, function(party){
        is_allowed_party = is.element(party, all_permitted_parties)
        if (!is_allowed_party) {
          warning(sprintf("%s is not permitted party name. Therefore, the prediction would not be calculated for coalition %s\n",
                          party,
                          paste(coalition, collapse = "-")))
        }
        return(is_allowed_party)
      })
      all(valid_party_names)
    })
    
    return(coalitions[are_with_permitted_parties])
  }
  
  assert_class(pv, c("pollyvote", "list"))
  assert_numeric(threshold, lower = 0)
  if (threshold >= 10) {
    warning(sprintf("Threshold is bigger than 10. It is equal to %d", threshold))
  }
  assert_choice(threshold_handle, c("omit", "ignore"))
  assert(
    check_numeric(election_year),
    check_null(election_year)
  )
  assert(
    check_character(permitted_parties),
    check_null(permitted_parties)
  )
  assert(
    check_character(region),
    check_null(region)
  )
  assert_numeric(limitdays)
  assert_logical(for.ggplot2)
  
  coalitions <- get_valid_coalitions(coalitions, pv, permitted_parties)
  
}

