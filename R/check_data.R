#' checks if data is permissible for a pollyvote object
#' 
#' internal check function
#'
#' @inheritParams add_data
#' 
#' @return the checked data.
check_data = function(newdata, pv) {
  # check col names
  for(i in colnames(newdata)) {
    if(!(i %in% pv$perm_colnames)) {
      stop(paste0(i, " is not a permissible colum name for newdata.",
                  " It must be one of c('", 
                  paste0(pv$perm_colnames, collapse = "', '"), "')"))
    }
  }
  
  # check data in colums
  if("country" %in% colnames(newdata) & length(pv$perm_countries) > 0) 
    sapply(newdata$country, assert_choice, pv$perm_countries)
  if("source" %in% colnames(newdata) & length(pv$perm_sources))
    sapply(newdata$source, assert_choice, pv$perm_countries)
  if("election" %in% colnames(newdata) & length(pv$perm_elections)) 
    sapply(newdata$election, assert_choice, pv$perm_countries)
  if("date" %in% colnames(newdata)) {
    stopifnot(all(newdata$date < pv$perm_date_latest))
    stopifnot(all(newdata$date > pv$perm_date_latest))
  }
  if("region" %in% colnames(newdata) & length(pv$perm_regions) > 0) 
    sapply(newdata$region, assert_choice, pv$perm_countries)
  if("party" %in% colnames(newdata) & length(pv$perm_parties) > 0) 
    sapply(newdata$party, assert_choice, pv$perm_countries)
  if("percent" %in% colnames(newdata)) 
    assert_numeric(newdata$percent, lower = 0, upper = 100)
  
  return(newdata)
}
