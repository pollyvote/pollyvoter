
#' aggregate percentage values
#' 
#' This is a function that aggregate percent values by groupping
#' variables group_variab.
#'
#' @param data  [\code{data.frame(n)}]\cr
#'     data.frame to be aggregated
#' @param group_variab  [\code{character(n)}]\cr
#'     grouping variables
#' @param aggr_func  [\code{character(n)}]\cr
#'     aggregation function     
#'
#' @examples
#' elect_data_wide <- import_exp_data_wide("Wahlumfrage")
#' elect_data_long <- wide_to_long_format(elect_data_wide)
#'
#' elect_data_aggr_mean <- aggreg_by(elect_data_long, group_variab = c("date", "party"), aggr_func = mean)
#' elect_data_aggr_max <- aggreg_by(elect_data_long, group_variab = c("date", "party"), aggr_func = max)
#' 
#' 
#' @return aggregated data.frame  
#'
#' @export
#' 
#' 

aggreg_by <- function(data, group_variab, aggr_func = mean){
  
  library(dplyr)
  
  # aggregate by group_variab
  data_aggr <- data %>% 
    group_by_(.dots = group_variab) %>% 
    summarize(aggreg_percent = aggr_func(percent, na.rm = TRUE))
  
  data_aggr
  
}





