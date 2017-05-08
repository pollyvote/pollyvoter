#' extract data
#' 
#' This function extracts the data from a pollyvote container.
#'
#' @param pv [\code{pollyvote(1)}]\cr
#'   the pollyvote object of which to extract the data from.
#'
#' @examples
#' pv = create_pollyvote()
#' # returns an empty data frame
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
#' @param id [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' get_parties("pollyvote")
#' 
#' @return party names  
#'
#' @export

get_parties <- function(id = "pollyvote"){
  
  # extract parties
  id$perm_parties 
  
}


#' extract regions
#' 
#' This function extract regions from a pollyvote container.
#'
#' @param id [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' get_regions("pollyvote")
#' 
#' @return regions  
#'
#' @export

get_regions <- function(id = "pollyvote"){
  
  # extract parties
  id$perm_regions 
  
}  


#' extract elections
#' 
#' This function extract elections from a pollyvote container.
#'
#' @param id [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' get_regions("pollyvote")
#' 
#' @return elections  
#'
#' @export

get_elections <- function(id = "pollyvote"){
  
  # extract parties
  id$perm_elections 
  
}  

  
  