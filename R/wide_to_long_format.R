#' @title convert election data set from wide to long format
#' 
#' @description This is a function that converts an election data set from the 
#' long  format into the wide format. Currently only German parties are supported.
#'
#' @param data  [\code{data.frame}]\cr
#'     data frame in wide fromat containing TODO
#'     
#' @importFrom tidyr gather
#' @return data.frame in long format 
#'
#' @export
wide_to_long_format <- function(data){
  
  library("tidyr")
  
  # transform to long format
  data_long <- gather(data, "party", "percent", 
                      one_of("cdu/csu", "spd", "grüne", "fdp", 
                             "linke", "piraten", "afd", "sonstige"))  
  data_long
  
}


