% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rjags.R
\name{complete_jags_fit}
\alias{complete_jags_fit}
\title{Complete JAGS Fit}
\usage{
complete_jags_fit(file, data, inits, jags_n, varnames)
}
\arguments{
\item{file, data, inits}{Passed to \code{\link[rjags:jags.model]{rjags::jags.model()}}.}

\item{jags_n}{named list. See \strong{JAGS Controls} section.}

\item{varnames}{character. Variable names to be monitored via JAGS.}
}
\value{
list with
\itemize{
\item \code{data}: same as input.
\item \code{model}: object returned from \code{\link[rjags:jags.model]{jags.model}}.
\item \code{output}: list of \code{\link[rjags:jags.samples]{mcarray}} objects.
\item \code{jags_n}: same as input.
}
}
\description{
Fits a model in JAGS (compilation, warm-up and sampling) with a modern
progress bar from \code{knitrProgressBar}.
}
\section{JAGS Controls}{

\itemize{
\item \code{n.adapt} controls number of discarded samples in adaptation stage.
\item \code{n.update} controls number of discarded samples in warm-up stage.
\item \code{n.iter} controls how many samples are saved.
\item \code{n.thin} controls thinning, where only every \eqn{n^th} sample is kept.
\item \code{n.chains} controls number of chains.
}
}

\keyword{internal}
