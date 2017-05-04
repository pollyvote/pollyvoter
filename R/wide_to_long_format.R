
#' convert election data set from wide to long format
#'
#' @param data A data.frame.
#' @return data.frame in long format 
#'
#' @docType package
#' @name pollyvoter
#' 
wide_to_long_format <- function(data){
  
  library("tidyr")
  
  # transform to long format
  data_long <- gather(data, party, percent, 
                      one_of("cdu/csu", "spd", "grüne", "fdp", 
                             "linke", "piraten", "afd", "sonstige"))  
  data_long
  
}


