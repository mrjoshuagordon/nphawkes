% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{getBackgroundT}
\alias{getBackgroundT}
\title{Computes the background probability associated with each point in the catalog}
\usage{
getBackgroundT(t, pbNew, nn, nbins_mu, grid_mu, delta_mu, n)
}
\arguments{
\item{t}{A vector of length n, holding the times of interest}

\item{pbNew}{A vector of length n, holding the background probabilities}

\item{nn}{A single number holding the nearest neighbor bandwith used in the kernel}

\item{nbins_mu}{A single number holding the number of time bins for the background rate}

\item{grid_mu}{A vector holding the grid positions for the time  bins of the background rate}

\item{delta_mu}{A single number holding the bin size of the time bins for the background rate}

\item{n}{A single integer indicating the number of events in the catalog}
}
\description{
Computes the background probability associated with each point in the catalog
}
