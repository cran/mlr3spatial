% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{factor_layer}
\alias{factor_layer}
\title{Factor Layer Generator}
\usage{
factor_layer(id, levels, in_memory = FALSE)
}
\arguments{
\item{id}{(\code{character(1)})\cr
Layer id.}

\item{levels}{(\code{character()})\cr
Factor levels.}

\item{in_memory}{(\code{logical(1)})\cr
If \code{FALSE} (default), layer is written to disk.}
}
\value{
Named \code{list()}
}
\description{
Generates a factor layer when passed to \code{\link[=generate_stack]{generate_stack()}}.
}
\keyword{internal}
