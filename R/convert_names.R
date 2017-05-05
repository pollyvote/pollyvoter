#' convert names to lower character and ascii
#' 
#' This is a function that converts a character vector to all lower case and ascii, see example.
#'
#' @param x  [\code{character(n)}]\cr
#'     character vector to be converted
#'
#' @examples
#' text = c("Grüne", "SPD")
#' convert_names(text)
#' 
#' @return data.frame in long format 
#'
#' @export
convert_names = function(x) {
  assert_character(x)
  iconv(x, from= "UTF-8", to = "ASCII//TRANSLIT") %>% (tolower)
}