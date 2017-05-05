

#' import example data
#' 
#' This is a function that import example election data in 
#' wide format.
#'
#' @param sheet  [\code{character(n)}]\cr
#'     excel sheet name
#'
#' @examples
#' elect_data_wide <- import_exp_data_wide("Wahlumfrage")
#' 
#' 
#' @return data.frame  
#'
#' @export
#' 
#' 

import_exp_data_wide <- function(sheet){
  
  library("readxl")
  # warning, this function seems to not work properly,
  individual.polls = readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                    package = "pollyvoter"), 
                                        sheet = sheet,
                                        skip = 1)
  assert_data_frame(individual.polls, min.cols = 2, min.rows = 2)
  
  colnames(individual.polls)[1] = "id"
  colnames(individual.polls)[3] = "survey.institute"
  colnames(individual.polls) = convert_names(colnames(individual.polls))
  
  # sort out empty rows and only the needed columns
  individual.polls = individual.polls[!is.na(individual.polls$id), 1:11]
  
  # coerce to numeric, get NAs
  individual.polls[, 4:11] = apply(individual.polls[, 4:11], 2, as.numeric) %>% suppressWarnings
  
  individual.polls
  
}

