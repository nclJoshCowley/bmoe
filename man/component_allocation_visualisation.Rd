% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/component-plots.R
\name{component_allocation_visualisation}
\alias{component_allocation_visualisation}
\title{Visualise Component Allocation}
\usage{
component_allocation_visualisation(object, chain)
}
\arguments{
\item{object}{Object that inherits from \code{\link{bmoe_fit}}.}

\item{chain}{integer. Specified chain(s), multiple chains forces list output.}
}
\description{
Show a raster plot where
\itemize{
\item each row is an observation;
\item each column is an individual MCMC iteration;
\item each tile is coloured according to component membership.
}
}
