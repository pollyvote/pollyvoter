#' extract data
#' 
#' This function extracts the data from a pollyvote container.
#'
#' @param pv [\code{pollyvote(1)}]\cr
#'   the pollyvote object of which to extract the data from.
#'
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' data("polls_individual")
#' pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
#'               source_type = "poll", election = "BTW")
#' get_data(pv)
#' 
#' @return data frame containing all the data stored in \code{pv}.  
#'
#' @export
get_data <- function(pv){
  assert_class(pv, "pollyvote")
  return(pv$data)
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


#' get an election result from a pollyvote object
#' 
#' gets an election result from a pollyvote object. To add a new election result to a pollyvote object
#' use \code{\link{add_election_result}}.
#'
#' @inheritParams add_election_result
#' 
#' @examples
#' pv = create_pollyvote(perm_countries = "D")
#' data("election_result")
#' pv = add_election_result(pv, "BTW 2013", election_result, date = "2013-09-22")
#' get_election_result(pv, election = "BTW 2013")
#' 
#' @return A data frame containing the election result.
#'
#' @export
get_election_result = function(pv, election, ...) {
  # check feasibility
  assert_class(pv, "pollyvote")
  assert_choice(election, names(pv$election_result))
  return(pv$election_result[[election]])
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
  