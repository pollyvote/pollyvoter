% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/error_calc.R
\name{error_calc}
\alias{error_calc}
\title{calculate the prediction error of a pollyvote prediction.}
\usage{
error_calc(object, method, ...)
}
\arguments{
\item{object}{[\code{pollyvote(1)}] \cr
pollyvote object to get the error calculation from.}

\item{method}{[\code{character(1)}] \cr
method name of the error calculation function of \code{object} to use.}

\item{...}{additional arguments to the error calculation function.}
}
\value{
a data frame containing the result of the error calculation
}
\description{
calculate the prediction error of a pollyvote prediction.
}
\examples{
pv = create_pollyvote(perm_countries = "D")
data("polls_individual")
pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
              source_type = "poll", election = "BTW")
data("election_result")
pv = add_election_result(pv, "BTW 2013", election_result, date = "2013-09-22")
error_calc_pred <- error_calc(pv, "prediction_election", 
                              prediction = "pollyvote", election = "BTW 2013")             

}
\seealso{
Other predict: \code{\link{error_calc.pollyvote}},
  \code{\link{predict.pollyvote}}
}
