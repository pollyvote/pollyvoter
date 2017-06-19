#' limit days in a data set
#' 
#' internal function that handles missing values in the data frame of a pollyvote 
#' object. Different error handling arguments can be handed over.
#' @param dat [\code{data.frame()}]\cr
#'   data frame of a pollyvote object.
#' @param na_handle [\code{character(1)}]\cr
#'   character describing which method is used to fill missing values.
#'   Currently supported is 
#'   \itemize{
#'   \item \code{'last'}: Use the last available value. Fill all days between the earliest and latest day in the data.
#'   \item \code{'omit'}: For every day, agregate available data.
#'   \item \code{'mean_within'} Use the available data of this subcomponent to fill missing values.
#'   \item \code{'mean_across'} Create a mini-pollyvote prediction by aggregating 
#'   all \code{source_type}s using \code{na_handle = 'last'} and then
#'   aggregating the result using \code{na_handle = 'last'}, which will then be used 
#'   to predict missing values in the lowest aggregation level.
#'   }
#' @param pv [\code{pollyvote}]\cr
#'   only required for \code{na_handle = 'mean_across'}, the pollyvote object containing the data frame.
#' @param ... [\code{list()}]\cr
#'   additional arguments, currently ignored.
#' 
#' @return the data set with filled mssing values.
#'
#' @export
limit_days = function(data, 
                      no_days, earliest_date,
                      election_data, 
                      pv, election, 
                      election_date, 
                      ...) {
  assert_class(data, "data.frame")
  # check if anything has to be done
  if (all(missing(no_days), missing(earliest_date),
          missing(election_data), 
          missing(pv), missing(election), 
          missing(election_date))) {
    return(data)
  }
  
  assert_numeric(no_days)
  if(!missing(earliest_date))
    earliest_date = as.POSIXct(earliest_date, format = "%Y-%m-%d")
  if(!missing(election_data))
    assert_class(election_data, "data.frame")
  if(!missing(pv))
    assert_class(pv, "pollyvote")
  if(!missing(election))
    assert_choice(election, names(pv$election_result))
  
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
  earliest_date = election_date - as.difftime(no_days, unit = "days")
  data = filter(data, date > earliest_date)
  return(data)
}
