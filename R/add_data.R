#' add data (to a pollyvote object)
#' 
#' Adds data, so far only implemented for pollyvote objects.
#'
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object to add the data to.
#' @param newdata [\code{data.frame}]\cr
#'   the data to add to the pollyvote object.
#' @param ... additional arguments giving information about \code{newdata}.
#' 
#' @return The pollyvote object with added data.
#'
#' @export
add_data = function(pv, newdata = data.frame(), ...) {
  UseMethod("add_data")
}

#' add data to a pollyvote object
#' 
#' Adds data to a pollyvote opbject.
#'
#' @inheritParams add_data
#' @importFrom plyr rbind.fill
#' 
#' @return The pollyvote object with added data.
#'
#' @export
add_data.pollyvote = function(pv, newdata = data.frame(),
                              ...) {
  # input checking
  assert_class(pv, "pollyvote")
  assert_data_frame(newdata)
  
  # run check on additional arguments
  check_additional_args(newdata, pv, ...)
  
  # add additional arguments to newdata
  args = list(...)
  newdata[,names(args)] = args
  
  # run checks on the data
  newdata = check_data(newdata, pv)
  
  # add data to pv
  # TODO maybe better with join/merge, in case the same data is added twice
  pv$data = plyr::rbind.fill(pv$data, newdata)
  
  return(pv)
}

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


#' checks additional arguments to add_data function
#' 
#' internal check function if additional arguments to pollyvoter::add_data are 
#' permissible for a pollyvote object
#'
#' @inheritParams add_data
#' @param ... additional named arguments to be checked.
#' 
#' @return The pollyvote object with added data.
#'
check_additional_args= function(newdata, pv, ...) {
  args = list(...)
  # check arg names with pv
  for(i in names(args)) {
    if(!(i %in% pv$perm_colnames)) {
      stop(paste0(i, " is not a permissible colum name for newdata.",
                  " It must be one of c('", 
                  paste0(pv$perm_colnames, collapse = "', '"), "')"))
    }
  }
  
  # check arg names with data
  for(i in names(args)) {
    if(i %in% colnames(newdata)) {
      warning(paste0(i, " is already a colum name in newdata."))
    }
  }
  
  # check length of additional arguments
  lapply(args, function(arg) {
    if(length(arg) != 1 | length(arg) == nrow(newdata)) {
      stop(paste("Argument", names(arg), " has to be of length 1 or ncol(newdata)."))
    }
  })
  return(NULL)
}
