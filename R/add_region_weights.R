#' Adds the region weights to the pollyvote container.
#' 
#' @param pv [\code{pollyvote}]\cr
#'   the pollyvote object to add region weights to.
#' @param region_weights [\code{data.frame}]\cr
#'   Data frame with columns 'region' and 'weight'
#'   representing region names and corresponding region weights.
#'   Region names are validated against the permitted regions in the pollyvote container.
#'   
#'   @examples 
#'   pv = add_region_weights(pv, region_weights)
#'   
#'   @return the pollyvote object with added region weights
#'   
#'   @export
add_region_weights = function(pv, region_weights) {
  
  assert_class(pv, "pollyvote")
  assert_data_frame(region_weights)
  if (nrow(region_weights) == 0)
    stop("The 'region_weights' dataframe has no data.
          Therefore, 'region_weights' can't be added")
  
  assert_character(region_weights$region)
  assert_numeric(region_weights$weight)
  if (length(pv$perm_regions) > 0)
    sapply(region_weights$region, assert_choice, pv$perm_regions)
  
  pv$region_weights = region_weights
  return(pv)
}