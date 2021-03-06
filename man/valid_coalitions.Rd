% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coalitions.R
\name{valid_coalitions}
\alias{valid_coalitions}
\title{Checks whether coalitions are made of permitted parties.
Permitted parties can be defined either in pv$permparties or in allowed_parties parameter.
Therefore, the idea is first to collect all specified permitted parties in one vector.
This vector will serve as a source for checking whether parties inside the coalitions have valid names.
If there are no parties specified, then the check for valid party names is not performed.}
\usage{
valid_coalitions(coalitions, pv, permitted_parties)
}
\arguments{
\item{coalitions}{[\code{list}]\cr
List of vectors representing coallitions.}

\item{pv}{[\code{pollyvote}]\cr 
The pollyvote object}

\item{permitted_parties}{[\code{character(n)}]\cr
Selection of only specific parties for which coalitions are calculated.}
}
\value{
list of coalitions consisted only of permitted parties.
}
\description{
Checks whether coalitions are made of permitted parties.
Permitted parties can be defined either in pv$permparties or in allowed_parties parameter.
Therefore, the idea is first to collect all specified permitted parties in one vector.
This vector will serve as a source for checking whether parties inside the coalitions have valid names.
If there are no parties specified, then the check for valid party names is not performed.
}
