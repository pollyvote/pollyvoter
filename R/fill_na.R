#' fill missing values in the data of a pollyvote object
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
fill_na = function(dat, na_handle = c("last", "omit", "mean_within", "mean_across"), pv = NULL, ... ) {
  na_handle = match.arg(na_handle)
  # na handling
  if (na_handle == "last") {
    preprocessed_dat = dat %>%
      complete(date,
               nesting(source, source_type, party)) %>%
      group_by(source, party) %>%
      fill(percent, .direction = "down")

  } else if (na_handle == "omit") {
    preprocessed_dat = dat

  } else if (na_handle == "mean_within") {
    preprocessed_dat = dat %>%
      complete(date,
               nesting(source, source_type, party)) %>%
      group_by(date, party) %>%
      mutate(percent = ifelse(is.na(percent), mean(percent, na.rm = T), percent))

  } else if (na_handle == "mean_across") {
    raw_dat = dat %>%
      complete(date,
               nesting(source, source_type, party))

    # create a small fake pollyvote prediction by
    # calculating the mean per source type and then over all aggregated source types
    across.dat = pv %>%
      get_data %>%
      group_by(date, source_type, party) %>%
      summarize(percent = mean(percent, na.rm = TRUE)) %>%
      group_by(date, party) %>%
      summarize(percent = mean(percent, na.rm = TRUE))

    preprocessed_dat = left_join(raw_dat, across.dat, by = c("date", "party"))
    preprocessed_dat = preprocessed_dat %>%
      mutate(percent = ifelse(is.na(percent.x), percent.y, percent.x))
  }
  return(preprocessed_dat)
}
