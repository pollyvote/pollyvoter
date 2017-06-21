#' limit days in a data set
#' 
#' internal function that handles missing values in the data frame of a pollyvote 
#' object. Different error handling arguments can be handed over.
#' 
#' @param data [\code{data.frame()}]\cr
#'   data frame of a pollyvote object.
#' @param no_days [\code{integer(1)}]\cr
#'   number of days before the election to return results for.
#' @param election_data [\code{data.frame()}]\cr
#'   data frame containing the election results. The election date can be included in the \code{date} coulmn.
#' @param pv [\code{pollyvot(1)}]\cr
#'   pollyvote container to extract the \code{election} from.
#' @param election [\code{character(1)}]\cr
#'   name of the election from \code{pv} to use.
#' @param election_date [\code{character(1)}]\cr
#'   date of the election in the format '\%Y-\%m-\%d'. If not supplied, the date of the election has to be supplied in the election data.
#' @param ... [\code{list()}]\cr
#'   additional arguments, currently ignored.
#' 
#' @return the data set limited to the number of days before the election.
#'
#' @export
limit_days = function(data, no_days, election_data, pv, election, 
                      election_date, ...) {
  assert_class(data, "data.frame")
  # check if anything has to be done
  if (all(missing(no_days), missing(election_data), missing(pv), 
          missing(election), missing(election_date))) {
    return(data)
  }
  
  assert_numeric(no_days)
  if(!missing(election_data))
    assert_class(election_data, "data.frame")
  if(!missing(pv))
    assert_class(pv, "pollyvote")
  if(!missing(election))
    assert_choice(election, names(pv$election_result))
  if(!missing(election_date))
    as.POSIXct(election_date, format = "%Y-%m-%d")
  
  # get election_data
  if (missing(election_data)){
    if (!missing(pv) & !missing(election))
      election_data = get_election_result(pv, election)
  }
  
  # get election date
  if (missing(election_date) & !missing(election_data)) {
    if ("date" %in% colnames(election_data)) {
      election_date = unique(election_data$date)
    } 
  } else {
    warning("limit_days could not find out the date of the election. Please specify the argument election_date.")
    return(data)
  }
  
  # subset the data accordingly
  earliest_date = election_date - as.difftime(no_days, units = "days")
  data = filter(data, date > earliest_date)
  return(data)
}
