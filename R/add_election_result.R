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
#'
#' @inheritParams add_election_result
#' 
#' @return The pollyvote object with added prediction
#'
#' @export
add_election_result.pollyvote = function(pv, election, data, ...) {
  # check feasibility
  if(!length(pv$perm_elections) == 0) assert_choice(election, pv$perm_elections)
  data = check_data(data, pv)
  
  # possibly overwrite election name
  data$election = election
  
  pv$election_result[[election]] = data
  return(pv)
}
