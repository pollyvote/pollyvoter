% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_pollyvote.R
\name{print.pollyvote}
\alias{print.pollyvote}
\title{print a pollyvote object}
\usage{
\method{print}{pollyvote}(x, ...)
}
\arguments{
\item{x}{a pollyvote object.}

\item{...}{currently ignored.}
}
\description{
internal method to print the most important features of a pollyvote container
}
\examples{
pv = create_pollyvote(perm_countries = "D")
print(pv)

}
