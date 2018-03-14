#' extract data
#' 
#' This function extracts the data from a pollyvote container.
#' If the time interval is specified, then the only the data that belongs to the input time interval is returned.
#' Otherwise, whole forecast data is returned.
#'
#' @param pv [\code{pollyvote(1)}]\cr
#'   the pollyvote object of which to extract the data from.
#' @param time_int[\code{time}]
#'   the time interval in which forecast data should be returned.
#'   e.g time_int = c("2016-10-31", "2017-11-09")
#'
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' data("polls_individual")
#' pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
#'               source_type = "poll", election = "BTW")
#' 
#' 
#' @return data frame containing data stored in \code{pv} possibly filtered by \code{election_year} parameter.  
#'
#' @export
get_data <- function(pv, time_int = NULL){
  
  if (is.null(time_int)) {
    return(pv$data)
  } else{
    time_int = strptime(time_int, format = "%Y-%m-%d")
    time_int = sort(time_int)
    assert_class(pv, "pollyvote")
    
    data = pv$data[pv$data$date > time_int[1] & pv$data$date <= time_int[2], ]
    if (nrow(data) == 0) {
      warning(paste("No prediction for time interval = ",  time_int[1], "-", time_int[2], "."))
    }
    data
  }
} 

#' Get's the region weights from the pollyvote container.
#' 
#' @param pv [\code{pollyvote(1)}]\cr
#'   the pollyvote object of which to extract the region weights from.
#' 
#' @examples 
#' region_weights = get_region_weights(pv)
#' 
#'  
#' @return the region region weights from the pollyvote container.
#' 
#' @export
get_region_weights = function(pv) {
  
  assert_class(pv, "pollyvote")
  region_weights = pv$region_weights
  assert_data_frame(region_weights)
  if (nrow(region_weights) == 0)
    stop("The 'region_weights' has no data.")
  
  return(region_weights)
}


#' extract party names
#' 
#' This function extract party names from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_countries = "D", perm_parties = c("CSU", "SPD"))
#' get_perm_parties(pv)
#'  
#' @return character vector containing all party names stored in \code{pv}.   
#'
#' @export
get_perm_parties <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_parties) 
}


#' extract regions
#' 
#' This function extract regions from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_regions = "national")
#' get_perm_regions(pv)
#' 
#' @return character vector containing all regions stored in \code{pv}. 
#'
#' @export
get_perm_regions <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_regions) 
}  


#' extract elections
#' 
#' This function extract elections from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_countries = "D", perm_elections = "BTW")
#' get_perm_elections(pv)
#' 
#' @return character vector containing all elections stored in \code{pv}.  
#'
#' @export
get_perm_elections <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_elections) 
}  


#' extract countries
#' 
#' This function extract countries from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' get_perm_countries(pv)
#' 
#' @return character vector containing all countries stored in \code{pv}.  
#'
#' @export
get_perm_countries <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_countries) 
}  


#' extract sources
#' 
#' This function extract sources from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_sources = "poll")
#' get_perm_sources(pv)
#' 
#' @return character vector containing all sources stored in \code{pv}.  
#'
#' @export
get_perm_sources <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_sources) 
}  


#' extract earliest date
#' 
#' This function extract earliest date from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_date_earliest = as.POSIXct( "2013-09-22"))
#' get_perm_date_earliest(pv)
#' 
#' @return character containing earliest date stored in \code{pv}.  
#'
#' @export
get_perm_date_earliest <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_date_earliest) 
}  


#' extract latest date
#' 
#' This function extract latest date from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_date_earliest = as.POSIXct( "2013-09-22"))
#' get_perm_date_latest(pv)
#' 
#' @return character containing latest date stored in \code{pv}.  
#'
#' @export
get_perm_date_latest <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_date_latest) 
}  


#' extract colnames
#' 
#' This function extract colnames from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' data("polls_individual")
#' pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
#'               source_type = "poll", election = "BTW")
#' get_perm_colnames(pv)
#' 
#' @return character containing colnames stored in \code{pv}.  
#'
#' @export
get_perm_colnames <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_colnames) 
}  


#' Get an election result from a pollyvote object
#' 
#' @param pv [\code{pollyvote}]\cr 
#'   The pollyvote object from which to get the election result.
#' @param election_date[\code{date(1)}]\cr 
#'   Election date by which to get the election result.
#' @param election_name[\code{character(1)}]\cr
#'   Election name by which to get the election result.
#' 
#' gets an election result from a pollyvote object. To add a new election result to a pollyvote object
#' use \code{\link{add_election_result}}.
#' 
#' If \code{election_date} and \code{election_result} are missing, whole data for elections is returned.
#'
#' @inheritParams add_election_result
#' 
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' data("election_result")
#' pv = add_election_result(pv, "BTW 2013", election_result)
#' get_election_result(pv, election_date = "2013-09-22")
#' get_election_result(pv, election = "BTW 2013")
#' 
#' @return A data frame containing the election result.
#'
#' @export
get_election_result = function(pv, election_date, election_name) {
  # check feasibility
  assert_class(pv, "pollyvote")
  
  if (missing(election_date) & missing(election_name))
    return(pv$election_result)

  if (missing(election_name)) {
    election_date = strptime(election_date, format = "%Y-%m-%d")
    if (is.na(election_date)) {
      stop("Election date must be in valid '%Y-%m-%d' format.")
    }
    result = pv$election_result[pv$election_result$date == election_date, ]
    if (nrow(result) == 0) 
      warning(paste("No election result exists for election date", election_date, sep = ": "))
    return (result)
  }
  if (missing(election_date)) {
    result = pv$election_result[pv$election_result$election == election_name, ]
    if (nrow(result) == 0) 
      warning(paste("No election result exists for election_name", election_name, sep = ": "))
    return (result)
  }
}

#' Get an election date that corresponds to a given \code{election_year}.
#' 
#' @param pv [\code{pollyvote}] the pollyvote object.\cr
#' @param election_year [\code{numeric(1)}] the election year.
#' If it is [\code{NULL}] (default value), latest election_date is returned.
#' 
#' @return An election date corresponding to the election_year.
#'
#' @export
get_election_date_from_election_year = function(pv, election_year = NULL) {
  
  assert_class(pv, "pollyvote")
  assert(
    check_numeric(election_year),
    check_null(election_year))
  
  election_results = get_election_result(pv)
  all_election_dates = unique(election_results$date)
  if (length(all_election_dates) == 0)
    stop("No election_results are present in the pollyvote object.")
  
  if (is.null(election_year))
    return(all_election_dates[length(all_election_dates)])
  
  idx = as.integer(format(all_election_dates, "%Y")) == election_year
  if (sum(idx) != 1)
    stop(paste("'election_year'", election_year, "does not exists in the pollyvote election results", sep = " "))
  
  all_election_dates[idx]
}

#' extract source types
#' 
#' This function extract source types from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_source_types = "poll")
#' get_perm_source_types(pv)
#' 
#' @return character vector containing all source types stored in \code{pv}.  
#'
#' @export
get_perm_source_types <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_source_types) 
}  


#' extract region types
#' 
#' This function extract region types from a pollyvote container.
#'
#' @param pv [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' pv = create_pollyvote(perm_region_types = "national")
#' get_perm_region_types(pv)
#' 
#' @return character vector containing all region types stored in \code{pv}.  
#'
#' @export
get_perm_region_types <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$perm_region_types) 
}  
  