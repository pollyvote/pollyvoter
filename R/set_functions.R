#' set data
#' 
#' This function completely overwrites the data from a pollyvote container.
#' Do not use unless you are sure that this is what you want.
#'
#' @param pv [\code{pollyvote(1)}]\cr
#'   the pollyvote object of which to overwrite the data from.
#' @param newdata [\code{data.frame}]\cr
#'   the data to replace the original data contained in \code{pv}.
#'
#' @examples
#' pv = create_pollyvote()
#' # returns an empty data frame
#' set_data(pv, newdata = data.frame(country = "D", election = "BTW"))
#' 
#' @return pollyvote object containing the new data.  
#'
#' @export
set_data <- function(pv, newdata){
  assert_class(pv, "pollyvote")
  # TODO write some sanity checks
  newdata = check_data(newdata, pv)
  pv$data = newdata
  return(pv)
}

# TODO correct those functions
#' #' set up party names
#' #' 
#' #' This function set up party names in a pollyvote container.
#' #'
#' #' @param parties [\code{character(1)}]\cr
#' #'   party names
#' #'   
#' #' @param id [\code{character(1)}]\cr
#' #'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#' #'
#' #' @examples
#' #' set_parties(parties = c("csu", "spd"), id = "pollyvote")
#' #' 
#' #' @return pollyvote container
#' #'
#' #' @export
#' 
#' set_parties <- function(parties, id = "pollyvote"){
#'   
#'   # input checking
#'   assert_character(parties)
#'   
#'   # set up parties
#'   id$perm_parties <- parties
#'   id
#'   
#' }
#' 
#' 
#' #' set up regions
#' #' 
#' #' This function set up regions in a pollyvote container.
#' #'
#' #' @param regions [\code{character(1)}]\cr
#' #'   regions
#' #'   
#' #' @param id [\code{character(1)}]\cr
#' #'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#' #'
#' #' @examples
#' #' set_regions(regions = c("bayern", "bawu"), id = "pollyvote")
#' #' 
#' #' @return pollyvote container
#' #'
#' #' @export
#' 
#' set_regions <- function(regions, id = "pollyvote"){
#'   
#'   # input checking
#'   assert_character(regions)
#'   
#'   # set up regions
#'   id$perm_regions <- regions
#'   id
#'   
#' }

