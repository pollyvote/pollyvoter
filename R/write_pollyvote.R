#' write a pollyvote prediction / error calculation
#'
#' writes the result of a prediction of a pollyvote object to a .csv file.
#'
#' @param x [\code{pollyvote(1)}] \cr
#'   pollyvote object to predict from.
#' @param file [\code{character(1)}] \cr
#'   either a character string naming a file or a connection open for writing. "" indicates output to the console.
#' @param method [\code{character(1)}] \cr
#'   function name of the write function to use. Currently supported are
#'   \code{'write.table'}, \code{'write.csv'} and \code{'write.csv2'}.
#' @param prediction [\code{character(1)}] \cr
#'   method of the prediction to be plotted.
#' @param error_calculation [\code{character(1)}] \cr
#'   method of the error_calculation to be plotted.
#' @param write_args [\code{list()}] \cr
#'   list of arguments to the write function specified in \code{method}.
#' @param ... additional arguments to the prediction or error calculation function.
#'
#' @return Nothing. A file is created.
#'
#' @family write
#'
#' @export
write.pollyvote = function(x, file = "", method = "write.table", prediction = NULL, error_calculation = NULL,
                           write_args = list(), ...) {
  assert_class(x, "pollyvote")
  assert_character(file)
  assert_choice(method, choices = c("write.table", "write.csv", "write.csv2"))
  assert_character(prediction, len = 1L, null.ok = TRUE)
  assert_character(error_calculation, len = 1L, null.ok = TRUE)

  if(is.null(prediction) & is.null(error_calculation))
    stop("Please specify either a prediction or an error calculation to save.")
  if(!is.null(prediction) & !is.null(error_calculation))
    stop("Please specify either a prediction or an error calculation to save, not both.")

  if(!is.null(prediction)) {
    save_dat = predict(x, prediction, ...)
  } else {
    save_dat = error_calc(x, error_calculation, ...)
  }
  do.call(method, c(list(x = save_dat, file = file), write_args))
}
