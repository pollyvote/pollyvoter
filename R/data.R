# this file is used to document the files in /data

#' individual polls of the BTW 2013
#'
#' A dataset in the long format containing the individual polls of several survey institutes over 
#' the course of a few weeks for the BTW 2013 in Germany.
#'
#' @format A data frame with 808 rows and 5 variables:
#' \describe{
#'   \item{id}{unique tag computed from \code{date} and \code{source}}
#'   \item{date}{Date of the poll.}
#'   \item{source}{Survey institute that conducted the survey.}
#'   \item{party}{Name of the party in all lower case.}
#'   \item{percent}{Share of the voters that are predicted to vote for this \code{party}}
#' }
#' @seealso \code{\link{poll_data_wide}}
"poll_data_long"

#' individual polls of the BTW 2013
#'
#' A dataset in the wide format containing the individual polls of several survey institutes over 
#' the course of a few weeks for the BTW 2013 in Germany.
#' \describe{
#'   \item{id}{unique tag computed from \code{date} and \code{source}}
#'   \item{date}{Date of the poll.}
#'   \item{source}{Survey institute that conducted the survey.}
#'   \item{cdu/cdu, spd, grune, fdp, linke, piraten, afd, sonstige}{Share of votes for this party in percent}
#' }
#' @format A data frame with 101 rows and 11 variables:
#' @seealso \code{\link{poll_data_long}}
"poll_data_wide"
