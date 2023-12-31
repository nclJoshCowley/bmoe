% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmoe.R
\name{subset_bmoe}
\alias{subset_bmoe}
\title{Subset MCMC Output}
\usage{
subset_bmoe(object, iters = NULL, chains = NULL)
}
\arguments{
\item{object}{Object that inherits from \code{\link{bmoe_fit}}.}

\item{iters}{integer. Vector of indices to keep, default includes all.}

\item{chains}{integer. Vector of chains to keep, default includes all.}
}
\description{
Subset the MCMC output and update details within a \code{\link[=bmoe]{bmoe()}} fit.
}
