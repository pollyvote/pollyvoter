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
#' @param limit_days [\code{numeric(1)}]\cr
#'   Limit in days before the election up to which the coalitions percentages are calculated.
#'   For example, specifying limitdays = 100 return coalitions percentages up to 100 days before the election.
#'   If negative number is supplied (default value of -1), then data from all days is taken into account when calculating coalition percentages.
#' @param for.ggplot2 [\code{logical(1)}]\cr
#'   Return format of coalitions predictions.
#'   Options:
#'    o FALSE(default) - Returns data frame of columns (date | Days to election | Coalition_1_percentage | ... | Coaltion_n_percentage)
#'    o TRUE - Data frame with rows containing visualisation points
#' 
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
  
  data = coalition_predictions_data(pv, election_year, limitdays)
  election_data = data[["election_data"]]
  election_date = data[["election_date"]]
  
  predictions = predict(pv, method = prediction) %>%
    filter(date %in% election_data$date & party %in% election_data$party) %>%
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
#' @param input coalitions to the function.
#' @param pv the pollyvote object.
#' @param allowed_parties the input allowed_parties object.
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



#' Filters coalition predictions data and election_results in order to extract the election_date.
#' 
#' Coalition predictions data from the pollyvote object is filtered based on 
#' the election_year and limit_days parameters.
#' 
#' @param pv [\code{pollyvote}]\cr 
#'   the pollyvote object.
#' @param election_year [\code{numeric(1)}]\cr
#'   the election_year for which to get coalition predictions data.
#' @param limit_days [\code{numeric(1)}]\cr
#'   limit in days before the election up to which the coalitions percentages are calculated.
#' 
#' @return list of election_date and coalitions predictions data filtered based on the election_year and limit_days parameters.
#' @export 
coalition_predictions_data = function(pv, election_year, limitdays){
  
  election_results = assert_list(pv$election_result)
  
  if (length(election_results) == 0) {
    stop("At least one election result must be present in the pollyvote object in order coalitions prediction to be computed")
  }
  
  #create data frame with columns: election_name|election_date|election_year
  elections_results_data = bind_rows(lapply(election_results, function(election_result) {
    election_name = election_result$election[[1]]
    election_date = election_result$date[[1]]
    if (is.null(election_date) || is.na(election_date)){
      # remove this if when date is mandatory to enter election date when entering results
      warning(sprintf("There is no year specified for election %s.", election_name))
      election_date = NA
    }
    election_year = as.numeric(format(as.POSIXct(election_date, format = "%Y-%m-%d"), "%Y"))
    
    data.frame(name = election_name, date = election_date, year = election_year, stringsAsFactors = FALSE)
  }))
  
  target_election_results_data = subset(elections_results_data, year == election_year)
  #if there is no year matching the election year, select the most recent one.
  if (nrow(target_election_results_data) == 0 ) {
    target_election_results_data = subset(elections_results_data, year == max(elections_results_data$year, na.rm = TRUE))
  }
  if (nrow(target_election_results_data) == 0 ) {
    stop("The election data can't be obtained from pollyvote object")
  }
  
  target_election_name = target_election_results_data$name[[1]]
  target_election_date = target_election_results_data$date[[1]]
  
  #get predictions data from the pollyvote object 
  election_data = get_data(pv)
  
  # If there is no election name defined for the polls data, don't subset the data.
  # This might cause a problems if there is data for more than one election in the pv$data object.
  if (is.element(target_election_name, election_data$election)) {
    election_data = subset(election_data, election == target_election_name)
  }
  
  if (limitdays > 0) {
    election_data = limit_days(election_data, limitdays, election_date = target_election_date)
  }
  
  if (nrow(election_data) == 0) {
    stop("No data found for elections. Coalitions predictions can't be made without data for elections")
  }
  
  list(election_date=target_election_date, election_data = election_data)
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