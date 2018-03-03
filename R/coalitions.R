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
#'   If positive, indicates the minimum voice component
#'   that a party has to reach in order to participate in a coalition.
#' @param threshold_handle [\code{character(1)}]\cr
#'   Specifies how to handle coalitions with parties that have less percentage than threshold.
#'   Options:
#'   \itemize{
#'    \item \code{'omit'}: default value. In this case, this coalition percentage is not calculated for the given date. Instead, NA is entered.
#'    \item \code{'ignore'}: In this case, the coalition will be made up of the remaining parties. 
#'    }
#' @param prediction [\code{character(1)}]\cr
#'   Name of the prediction function.
#'   The Component definition deviates here somewhat from the other declarations to the Normally to come closer.
#'   Default value is the string specification of the root prediction, ie"Pollyvote", at a transversal (i.e. non-regional) level.
#'   Options:
#'   \itemize{
#'    \item [\code{character(1)}]: Single component name (e.g., "poll"); in this case will one column for the specified component
#'      and one column each (direct) Subcomponent returned.
#'    \item [\code{character(n)}]: Vector with component names - gives exactly for the specified components (without automatic subcomponent addition) the values back.
#'    \item [\code{NULL}]: Wildcard that returns all components.
#'    }
#' @param election_year [\code{numeric(1)}]\cr
#'   The election year for which the coalitions are predicted.
#'   If not specified, the most recent election year is used.
#' @param permitted_parties [\code{character(n)}]\cr
#'   Selection of only specific parties for which coalitions are calculated.
#'   Options:
#'   \itemize{
#'    \item [\code{NULL}]: Default value for not excluding any party in coalitions calculations.
#'    \item [\code{character(1)}]: For specifying one party.
#'    \item [\code{character(n)}]: Vector for specifying multiple parties.
#'    }
#' @param region [\code{character(1)}]\cr
#'   The region for which to calculate coalitions predictions.
#'   If the specified region is not defined, error is thrown.
#'   Options:
#'   \itemize{
#'    \item [\code{NULL}] - Default value. If the region is not specified, then the result over all regions is returned.
#'    \item [\code{character(1)}]: The Specified region for which coalitions are calculated.
#' @param limit_days [\code{numeric(1)}]\cr
#'   Limit in days before the election up to which the coalitions percentages are calculated.
#'   For example, specifying limitdays = 100 return coalitions percentages up to 100 days before the election.
#'   If negative number is supplied (default value of -1), then data from all days is taken into account when calculating coalition percentages.
#' @param for.ggplot2 [\code{logical(1)}]\cr
#'   Return format of coalitions predictions.
#'   Options:
#'   \itemize{
#'    \item [\code{logical(1)}] FALSE: Default value - Function returns data frame of columns (date | Days to election | Coalition_1_percentage | ... | Coaltion_n_percentage)
#'    \item [\code{logical(1)}] TRUE:  Data frame with rows containing visualisation points
#'    }
#' @return dataframe of columns (date | Days to election | Coalition_1_percentage | ... | Coaltion_n_percentage)
#'   or visualisation points of the coalitions prediction.
#' @export
calc_coalitions = function(pv, coalitions, threshold = 0, threshold_handle = 'omit', prediction ='pollyvote',
                           election_year = NULL, permitted_parties = NULL, region = NULL, limitdays = -1, for.ggplot2 = FALSE ) {
  
  
  coalitions = lapply(coalitions, function(coalition) {
    convert_names(coalition)
  })
  # use only permitted parties 
  coalitions = valid_coalitions(coalitions, pv, permitted_parties)
  
  assert_class(pv, c("pollyvote", "list"))
  assert_numeric(threshold, lower = 0)
  if (threshold >= 10) {
    warning(sprintf("Threshold is bigger than 10. It is equal to %d", threshold))
  }
  assert_choice(threshold_handle, c("omit", "ignore"))
  assert(
    check_numeric(election_year),
    check_null(election_year))
  assert(
    check_character(permitted_parties),
    check_null(permitted_parties))
  assert(
    check_character(region),
    check_null(region))
  assert_numeric(limitdays)
  assert_logical(for.ggplot2)
  
  prediction_time_int <- prediction_time_int(pv, election_year)
  limitdays = ifelse(limitdays < 0, Inf, limitdays)
  election_date = get_election_date_from_election_year(pv, election_year)
  
  predictions = predict(pv, time_int = prediction_time_int, method = prediction) %>%
    limit_days(no_days = limitdays, pv = pv, election_date = election_date)%>%
    threshold_and_replace_party_with_coalition(threshold, threshold_handle, coalitions) %>%
    group_by(date, party) %>%
    summarise(percent = sum(percent)) %>%
    mutate(days_to_election = as.numeric(difftime(election_date, date, units="days")))
  
  names(predictions)[names(predictions) == "party"] = "coalition"
  
  #transform data in appropriate response format
  if (!for.ggplot2) {
    result = data.frame(date = unique(predictions$date), days_to_election = unique(predictions$days_to_election))
    predictions = spread(predictions, coalition, percent)
  }
  
  predictions %>% arrange(days_to_election)
}

#' Checks whether coalitions are made of permitted parties.
#' Permitted parties can be defined either in pv$permparties or in allowed_parties parameter.
#' Therefore, the idea is first to collect all specified permitted parties in one vector.
#' This vector will serve as a source for checking whether parties inside the coalitions have valid names.
#' If there are no parties specified, then the check for valid party names is not performed.
#' 
#' @param coalitions [\code{list}]\cr
#'   List of vectors representing coallitions.
#' @param pv [\code{pollyvote}]\cr 
#'   The pollyvote object
#' @param permitted_parties [\code{character(n)}]\cr
#'   Selection of only specific parties for which coalitions are calculated.
#' 
#' @return list of coalitions consisted only of permitted parties.
#' @export   
valid_coalitions = function(coalitions, pv, permitted_parties){
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
                        paste(coalition, collapse = "_")))
      }
      return(is_allowed_party)
    })
    all(valid_party_names)
  })
  
  coalitions[are_with_permitted_parties]
}

#' Gets the prediction time interval based on the chosen election year.
#' The time interval should include the predictions data
#' after the previous election and target election.
#' 
#' @param pv [\code{pollyvote}]\cr 
#'   The pollyvote object
#' @param election_year [\code{numeric(1)}]\cr
#'   The election year for which the coalitions are predicted.
#'   If not specified, the most recent election year is used.
#'   
#' @return prediction data time interval
#' @export
prediction_time_int = function(pv, election_year) {
  
  assert_class(pv, "pollyvote")
  assert(
    check_numeric(election_year),
    check_null(election_year))
  
  target_election_date = get_election_date_from_election_year(pv, election_year)
  all_election_dates = sort(unique(get_election_result(pv)$date))
  if (target_election_date == all_election_dates[1]) {
    dummy_date = target_election_date - as.difftime(365, units = "days")
    return(c(dummy_date, target_election_date))
  }
  
  c(all_election_dates[all_election_dates == target_election_date - 1], target_election_date)
}

#' Applies the threshold rule and replaces the parties in the election_data with coalitions names.
#' 
#' For each day, it is checked whether data is available for all parties in the coalition.
#' If not, if threshold is greater than zero, NA or zero is inserted depending on the threshold_handle value.
#' 
#' @param data [\code{data.frame}]\cr 
#'   the elections data frame.
#' @param threshold [\code{numeric(1)}]\cr
#'   If positive, indicates the minimum voice component
#'   that a party has to reach in order to participate in a coalition.
#' @param threshold_handle [\code{character(1)}]\cr
#'   Specifies how to handle coalitions with parties that have less percentage than threshold.
#' @param coalitions [\code{list}]\cr
#'   The coalitions for which prediction is calculated.
#'   
#' @return data with applied threshold rule and replaced party names with coalition names.
#' @export 
threshold_and_replace_party_with_coalition = function(data, threshold, threshold_handle, coalitions) {
  
  coalitions_data = list()
  for (i in 1:length(coalitions)) {
    coalition_data_by_days = lapply(split(data, data$date), function(data_by_date) {
      threshold_percent = function(percent, data_by_date) {
        if (threshold_handle == 'omit') {
          are_all_parties_present = all(coalitions[[i]] %in% data_by_date$party)
          # If some party of coalition is missing for a given day and threshold_handle = "omit" then
          # all values for the parties in coalition for that day are set to NA.
          if (!are_all_parties_present) {
            return(rep(NA, times = length(percent)))
          }
          percent[percent < threshold] = NA        
        } else {
          percent[percent < threshold | is.na(percent)] = 0
        }
        
        return(percent)
      }
      
      data_by_date %>%
        filter(party %in% coalitions[[i]]) %>%
        mutate(percent = threshold_percent(percent, data_by_date), party = paste(coalitions[[i]], collapse = "_"))
    })
    
    coalitions_data[[i]] = bind_rows(coalition_data_by_days)
  }
  
  bind_rows(coalitions_data)
}