#' Adds the region weights to the pollyvote container.
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object to add region weights to.
#' @param regions [\code{character(n)}]\cr
#'   Vector of region names
#'   Region names are validated against the permitted regions in the pollyvote container.
#' @param weights [\code{numeric(n)}]\cr
#'   Vector of region weights. If \code{NULL}, then all weights are assigned to 1 (Default value).
#'   
#' @examples 
#'   pv = add_regions(pv, c("London", "Yorkshire"), c(0.7, 0.3))
#'   pv = add_regions(pv, c("London", "Yorkshire"))
#'   
#' @return the pollyvote object with added region weights
#'   
#' @export
#'   
add_regions = function(pv, regions, weights = NULL) {
  
  #validation
  assert_class(pv, "pollyvote")
  assert_character(regions)
  regions_length <- length(regions)
  if (regions_length == 0)
    stop("Regions length is 0.")
  
  if (!is.null(weights)) {
    assert_numeric(weights)
    if (length(weights) == 0)
      stop("Weights lentgth is 0.")
    if (regions_length != length(weights)) {
      stop("Regions and the corresponding weigths must be of same length.")
    }
  } else {
    weights = rep(1, times = regions_length)
  }
  
  if (length(pv$perm_regions) > 0)
    sapply(regions, assert_choice, pv$perm_regions)
  
  pv$region_weights = data.frame(region = regions, weight = weights, stringsAsFactors = FALSE)
  return(pv)
}