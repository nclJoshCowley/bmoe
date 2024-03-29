% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-data.R
\name{bmoe-extract-data}
\alias{bmoe-extract-data}
\alias{extract_x_regr}
\alias{extract_x_wt}
\alias{extract_y}
\title{Extract Data from Model Fit}
\usage{
extract_x_regr(object, new_data)

extract_x_wt(object, new_data)

extract_y(object, new_data)
}
\arguments{
\item{object}{Object that inherits from \code{\link{bmoe_fit}}.}

\item{new_data}{data frame.
Similar structure to original \code{data} argument.

Setting \code{new_data = NULL} signals for the observed data to be used.}
}
\value{
\code{extract_y()} returns a list of vectors, one per \code{y} variable.
}
\description{
Getter functions for the data used in a \code{bmoe} model fit.
}
