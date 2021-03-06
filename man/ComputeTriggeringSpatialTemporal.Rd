% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{ComputeTriggeringSpatialTemporal}
\alias{ComputeTriggeringSpatialTemporal}
\title{Computes the triggering function for a spatial temporal hawkes point process with a homonogenous and stationary background rate.}
\usage{
ComputeTriggeringSpatialTemporal(g, f, mu, n)
}
\arguments{
\item{g}{A vector of length n(n-1)/2, holding the triggering probabilities for time}

\item{f}{A vector of length n(n-1)/2, holding the triggering probabilities for distance}

\item{mu}{A single numeric of length 1, holding the background rate}

\item{n}{A single integer indicating the number of events in the catalog.}
}
\value{
pbNew A vector of background probabilities

ptNew A vector of triggering probabilities
}
\description{
Computes the triggering function for a spatial temporal hawkes point process with a homonogenous and stationary background rate.
}
