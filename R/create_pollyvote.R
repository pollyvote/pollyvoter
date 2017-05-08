#' create a pollyvote data container
#' 
#' A pollyvote container is a data frame containing all the information that is 
#' needed to predict an election.
#'
#' @param id [\code{character(1)}]\cr
#'   the name ID of the pollyvote object, defaults to 'pollyvote'.
#' @param perm_countries,perm_sources,perm_elections,perm_regions,perm_parties [\code{character(n)}]\cr
#'   permitted values for the respective column of the data of this pollyvote object
#' @param perm_date_earliest,perm_date_latest [\code{POSIXct(1)}]\cr
#'   earliest and latest permissible date in format \%Y-\%m-\%d.
#' @param perm_colnames [\code{character(n)}]\cr
#'   column names that are permitted in the data of this pollyvote object
#'
#' @return object of class pollyvote, containing an id, an initially empty data frame
#'   and potentially permissible values for the entries of the data frame.
#'   
#' @export
create_pollyvote = function(id = "pollyvote",
                            perm_countries = character(0), 
                            perm_sources = character(0), 
                            perm_elections = character(0), 
                            perm_date_earliest = NULL, 
                            perm_date_latest = NULL,
                            perm_regions = character(0), 
                            perm_parties = character(0),
                            perm_colnames = c("id", "country", "source", 
                                              "election", "date", "region", 
                                              "party", "percent")) {
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
  assert_character(perm_countries)
  assert_character(perm_parties)
  assert_character(perm_colnames)
  
  # initiate data
  data = data.frame(id = character(), 
                    country = character(), 
                    source = character(), 
                    election = character(),
                    date = character(), 
                    region = character(), 
                    party = character(),
                    percent = numeric())
  
  # crate pollyvote object
  pv = list(id = id,
            perm_countries = perm_countries, 
            perm_sources = perm_sources, 
            perm_elections = perm_elections, 
            perm_date_earliest = perm_date_earliest, 
            perm_date_latest = perm_date_latest,
            perm_regions = perm_regions, 
            perm_parties = perm_parties,
            perm_colnames = perm_colnames,
            data = data)
  class(pv) = c("pollyvote", "list")
  return(pv)
}


#' print a pollyvote data container
#' 
#' internal method to print out the most important features of a pollyvote container
#' 
#' @param pv a pollyvote object.
print.pollyvote = function(pv) {
  print.listof(pv)
}
