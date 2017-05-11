# this file is used to document the files in /data

#' "Individual polls" from source type "polls" (BTW 2013)
#'
#' A dataset in the long format containing polls ("Individual polls") polls from source type "polls" 
#' for the BTW 2013 in Germany.
#'
#' @format A data frame with 808 rows and 5 variables:
#' \describe{
#'   \item{id}{unique tag computed from \code{date} and \code{source}}
#'   \item{date}{Date of the poll.}
#'   \item{source}{Survey institute that conducted the survey.}
#'   \item{party}{Name of the party in all lower case.}
#'   \item{percent}{Share of the voters that are predicted to vote for this \code{party}}
#' }
#' @seealso \code{\link{polls_individual}}
#' @family example data
"polls_individual"


#' "Wahlumfrage" from source type "polls" (BTW 2013)
#'
#' A dataset in the long format containing polls ("Wahlumfrage") from source type "polls" 
#' for the BTW 2013 in Germany.
#'
#' @format A data frame with 1048 rows and 3 variables:
#' \describe{
#'   \item{date}{Date of the poll.}
#'   \item{party}{Name of the party in all lower case.}
#'   \item{percent}{Share of the voters that are predicted to vote for this \code{party}}
#' }
#' @seealso \code{\link{polls_individual}}, \code{\link{polls_tix}}
#' @family example data
"polls_wahlumfrage"


#' "PollyTix" from source type "polls" (BTW 2013)
#'
#' A dataset in the long format containing polls ("PollyTix") from source type "polls" 
#' for the BTW 2013 in Germany.
#'
#' @format A data frame with 1120 rows and 3 variables:
#' \describe{
#'   \item{date}{Date of the poll.}
#'   \item{party}{Name of the party in all lower case.}
#'   \item{percent}{Share of the voters that are predicted to vote for this \code{party}}
#' }
#' @seealso \code{\link{polls_individual}}, \code{\link{polls_wahlumfrage}}
"polls_tix"


#' "Eix" from source type "markets" (BTW 2013)
#'
#' A dataset in the long format containing election data ("Eix") from source type "markets" 
#' for the BTW 2013 in Germany.
#'
#' @format A data frame with 1104 rows and 3 variables:
#' \describe{
#'   \item{date}{Date of the poll.}
#'   \item{party}{Name of the party in all lower case.}
#'   \item{percent}{Share of the voters that are predicted to vote for this \code{party}}
#' }
#' @seealso \code{\link{markets_prognosys}}, \code{\link{markets_wahlfieber_1}}
"markets_eix"


#' "Prognosys" from source type "markets" (BTW 2013)
#'
#' A dataset in the long format containing election data ("Prognosys") from source type "markets" 
#' for the BTW 2013 in Germany.
#'
#' @format A data frame with 992 rows and 3 variables:
#' \describe{
#'   \item{date}{Date of the poll.}
#'   \item{party}{Name of the party in all lower case.}
#'   \item{percent}{Share of the voters that are predicted to vote for this \code{party}}
#' }
#' @seealso \code{\link{markets_eix}}, \code{\link{markets_wahlfieber_1}}
"markets_prognosys"


#' "Wahlfieber 1" from source type "markets" (BTW 2013)
#'
#' A dataset in the long format containing election data ("Wahlfieber 1") from source type "markets" 
#' for the BTW 2013 in Germany.
#'
#' @format A data frame with 632 rows and 3 variables:
#' \describe{
#'   \item{date}{Date of the poll.}
#'   \item{party}{Name of the party in all lower case.}
#'   \item{percent}{Share of the voters that are predicted to vote for this \code{party}}
#' }
#' @seealso \code{\link{markets_eix}}, \code{\link{markets_prognosys}}
"markets_wahlfieber_1"


#' "Election result" (BTW 2013)
#'
#' A dataset in the long format containing election result for the BTW 2013 in Germany.
#'
#' @format A data frame with 8 rows and 2 variables:
#' \describe{
#'   \item{party}{Name of the party in all lower case.}
#'   \item{percent}{Share of the voters that are predicted to vote for this \code{party}}
#'   \item{gov}{Indicates whether \code{party} is a governing party or not}
#' }
"election_result"
