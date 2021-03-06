% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initial_pollyvote_functions.R
\name{validate_common_prediction_params}
\alias{validate_common_prediction_params}
\title{Validates common input parameters to the prediction functions.}
\usage{
validate_common_prediction_params(pv, agg_fun, na_handle)
}
\arguments{
\item{pv}{[\code{pollyvote}]\cr
the pollyvote object of which to get the prediction data from.}

\item{agg_fun}{[\code{character(1)}]\cr
the name of the aggregation function to use, currently 'mean' and 'median' are supported}

\item{na_handle}{[\code{character(1)}]\cr 
specifies how NA values are handled in the data.}

\item{time_int[\code{date}]}{the time interval for the prediction.}
}
\description{
Validates common input parameters to the prediction functions.
}
