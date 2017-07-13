#' create a pollyvote data container
#' 
#' A pollyvote container is a data frame containing all the information that is 
#' needed to predict an election.
#'
#' @param id [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#' @param perm_countries,perm_sources,perm_source_types,perm_elections,perm_regions,perm_region_types,perm_parties [\code{character(n)}]\cr
#'   permitted values for the respective column of the data of this pollyvote object
#' @param perm_date_earliest,perm_date_latest [\code{POSIXct(1)}]\cr
#'   earliest and latest permissible date in format \%Y-\%m-\%d.
#'
#' @return object of class pollyvote, containing an id, an initially empty data frame
#'   and potentially permissible values for the entries of the data frame.
#'
#' @section initial functions:
#'   some useful prediction, aggregation and error calculation functions are already
#'   initialized when calling \code{create_pollyvote()}. 
#'   These are:
#'   \itemize{
#'     \item prediction functions
#'       \itemize{
#'       \item 'pollyvote': \code{\link{initial_prediction_pollyvote}}
#'       \item 'aggr_source_type'
#'     }
#'     \item error calculations: 'prediction_election'
#'     }
#' @importFrom stats qnorm
#' @export
create_pollyvote = function(id = "pollyvote",
                            perm_countries = character(0), 
                            perm_sources = character(0),
                            perm_source_types = character(0),
                            perm_elections = character(0), 
                            perm_date_earliest = NULL, 
                            perm_date_latest = NULL,
                            perm_regions = character(0),
                            perm_region_types = character(0),
                            perm_parties = character(0)
) {
  # input checking
  assert_character(id)
  assert_character(perm_countries)
  assert_character(perm_sources)
  assert_character(perm_elections)
  if(!is.null(perm_date_earliest)) {
    assert_class(perm_date_earliest, "POSIXct")
    date = format(perm_date_earliest, format = "%Y-%m-%d", usetz = FALSE)
  }
  if(!is.null(perm_date_latest)) {
    assert_class(perm_date_latest, "POSIXct")
    date = format(perm_date_latest, format = "%Y-%m-%d", usetz = FALSE)
  }
  assert_character(perm_regions)
  assert_character(perm_region_types)
  assert_character(perm_countries)
  assert_character(perm_parties)
  
  
  # initiate data
  data = data.frame(id = character(), 
                    country = character(), 
                    source = character(), 
                    source_type = character(),
                    election = character(),
                    # date = character(),
                    date = as.POSIXct(format(character(0), format = "%Y-%m-%d", usetz = FALSE)), # TODO format
                    region = character(), 
                    region_type = character(),
                    party = character(),
                    percent = numeric(),
                    government = numeric())
  
  # initiate permitted col names for data
  perm_colnames = c("id", "country", 
                    "source", "source_type",
                    "election", "date", 
                    "region", "region_type", 
                    "party", "percent", "government")
  
  # crate pollyvote object
  pv = list(id = id,
            perm_countries = perm_countries, 
            perm_sources = perm_sources, 
            perm_source_types = perm_source_types,
            perm_elections = perm_elections, 
            perm_date_earliest = perm_date_earliest, 
            perm_date_latest = perm_date_latest,
            perm_regions = perm_regions, 
            perm_region_types = perm_region_types,
            perm_parties = perm_parties,
            perm_colnames = perm_colnames,
            predictions = list(),
            election_result = list(),
            error_calc = list(),
            data = data)
  class(pv) = c("pollyvote", "list")
  
  # initialize some useful prediction, aggregation and error calculation functions
  # TODO describe all of them in initial_pollyvote_functions.R
  # work analogously to initial_prediction_pollyvote
  pv = add_prediction(pv, "pollyvote", initial_prediction_pollyvote)
  
  pv = add_prediction(pv, "aggr_source_type", initial_prediction_aggr_source_type)
  
  # error calculation based on name of prediction and election
  pv = add_error_calc(pv, "prediction_election", function(pv, prediction = "pollyvote", election, 
                                                          ci = FALSE, alpha = 0.05, no_days = Inf, ... ) {
    
    # extract election result
    if (length(pv$election_result) == 0)
      stop("pv does not contain any election results. Use add_election_result() to add the results of an election.")
    if (missing(election)) 
      election = names(pv$election_result)[1]
    result = get_election_result(pv, election)
    
    # extract predicted data
    pred_data = predict(pv, method = prediction, ...) %>%
      limit_days(no_days = no_days,
                 election_data = result, ...)
    
    # bring the prediction and the result together
    joined = left_join(x = pred_data, y = result, by = "party") %>%
      #rename(percent = percent.x, percent.true = percent.y)
      ungroup %>%
      rename(percent = percent.x, percent.true = percent.y,
             date = date.x, election_date = date.y)
    error_dat = mutate(joined, error = abs(percent - percent.true))
    
    if(!ci) {
      return(error_dat)
    } else {
      ec_mean_error = error_dat %>% 
        group_by(party) %>%
        summarize(mean_error = mean(error))
      ec_ci = left_join(error_dat, ec_mean_error, by = "party") %>%
        mutate(ci_lower = percent - qnorm(1 - alpha / 2) * mean_error,
               ci_upper = percent + qnorm(1 - alpha / 2) * mean_error)
      return(ec_ci)
    }
    
  })
  return(pv)
}


#' print a pollyvote object
#' 
#' internal method to print the most important features of a pollyvote container
#' 
#' @param x a pollyvote object.
#' @param ... currently ignored.
#' @export
print.pollyvote = function(x, ...) {
  cat("\n")
  cat("\t pollyvote object \n")
  cat("\n")
  if(length(get_perm_countries(x)) != 0)
    cat("permitted countries:", get_perm_countries(x), "\n")
  if(length(get_perm_source_types(x)) != 0)
    cat("permitted source types:", get_perm_sources(x), "\n")
  if(!is.null(get_perm_date_earliest(x)) & !is.null(get_perm_date_earliest(x)))
    cat("Permitted dates: From", get_perm_date_earliest(x), " to ", get_perm_date_latest(x), "\n")
  if(length(get_perm_regions(x)) != 0)
    cat("permitted regions:", get_perm_regions(x), "\n")
  if(length(get_perm_region_types(x)) != 0)
    cat("permitted region types:", get_perm_region_types(x), "\n")
  if(length(get_perm_parties(x)) != 0)
    cat("permitted parties:", get_perm_parties(x), "\n")
  
  dims = dim(get_data(x))
  cat("data:", dims[1], " observations on", length(unique(get_data(x)$date)), "days. \n")
  if(!is.null(names(x$predictions)))
    cat("available predictions:", names(x$predictions), "\n")
  if(!is.null(names(x$election_result)))
    cat("available elections:", names(x$election_result), "\n")
  if(!is.null(names(x$error_calc)))
    cat("available error calculations:", names(x$error_calc), "\n")
  cat("\n")
}
