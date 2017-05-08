#' set up party names
#' 
#' This function set up party names in a pollyvote container.
#'
#' @param parties [\code{character(1)}]\cr
#'   party names
#'   
#' @param id [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' set_parties(parties = c("csu", "spd"), id = "pollyvote")
#' 
#' @return pollyvote container
#'
#' @export

set_parties <- function(parties, id = "pollyvote"){
  
  # input checking
  assert_character(parties)
  
  # set up parties
  id$perm_parties <- parties
  id
  
}


#' set up regions
#' 
#' This function set up regions in a pollyvote container.
#'
#' @param regions [\code{character(1)}]\cr
#'   regions
#'   
#' @param id [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#'
#' @examples
#' set_regions(regions = c("bayern", "bawu"), id = "pollyvote")
#' 
#' @return pollyvote container
#'
#' @export

set_regions <- function(regions, id = "pollyvote"){
  
  # input checking
  assert_character(regions)
  
  # set up regions
  id$perm_regions <- regions
  id
  
}

