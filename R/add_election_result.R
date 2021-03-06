#' add election results (to a pollyvote object)
#' 
#' Adds an election result, so far only implemented for pollyvote objects.
#'
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object to add the election results to.
#' @param election [\code{character(1)}]\cr
#'   name of the election. One of the permitted elections of \code{pv}.
#' @param data [\code{data.frame()}]\cr
#'   data frame containing the election results in the long format.
#' @param ... currently ignored.
#' 
#' @examples
#' pv = create_pollyvote(perm_countries = "D") 
#' data("election_result")
#' pv = add_election_result(pv, "BTW 2013", election_result, date = "2013-09-22")
#' 
#' 
#' @return The pollyvote object with added prediction.
#'
#' @export
add_election_result = function(pv, election, data, ...) {
  assert_class(pv, "pollyvote")
  UseMethod("add_election_result")
}

#' add an election result to a pollyvote object
#' 
#' Adds election result to a pollyvote opbject.
#' The election date must be in format "%Y-%m-%d" and of type POSIXlt', 'POSIXct' or 'character'.
#'
#' @inheritParams add_election_result
#' 
#' @examples
#' pv = create_pollyvote(perm_countries = "D") 
#' data("election_result")
#' pv = add_election_result(pv, "BTW 2013", election_result)
#' 
#' @return The pollyvote object with added prediction
#'
#' @export
add_election_result.pollyvote = function(pv, election, data, ...) {
  # input checking
  assert_class(pv, "pollyvote")
  assert_data_frame(data)
  election_date = data$date
  if (!(is(election_date, "POSIXlt") | is(election_date, "POSIXct") | is.character(election_date)))
    stop("Date of election must be either 'POSIXlt', 'POSIXct' or 'character'")
  
  # run check on additional arguments
  check_additional_args(data, pv, ...)
  # add additional arguments to newdata
  args = list(...)
  data[,names(args)] = args
  
  # check feasibility
  if(!length(pv$perm_elections) == 0) 
    assert_choice(election, pv$perm_elections)
  data = check_data(data, pv)
  
  # possibly overwrite election name
  data$election = election
  
  pv$election_result = plyr::rbind.fill(pv$election_result, data)
  return(pv)
}
