% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.nphawkesMSTNH.R
\name{plot.nphawkesMSTNH}
\alias{plot.nphawkesMSTNH}
\title{Function to plot the magnitude productivity, spatial, temporal, and background components of the Hawkes model}
\usage{
\method{plot}{nphawkesMSTNH}(x, type = "time", print = FALSE, ...)
}
\arguments{
\item{x}{An object of class nphawkesMSTH}

\item{type}{A string indicating the type of plot to generate}

\item{print}{A logical indicating whether the plot should be printed or returned}

\item{...}{Other parameters passed in}
}
\value{
p A ggplot2 plot
}
\description{
Function to plot the magnitude productivity, spatial, temporal, and background components of the Hawkes model
}
\examples{
data(catalog)
data <- nphData(data = catalog[catalog$Magnitude > 4.0,], time_var = 'tdiff', x_var = 'Longitude', y_var = 'Latitude', mag = 'Magnitude')
fit <- nphawkesMSTNH(data = data, np = 100, eps = 1e-2, xrange = c(-127, -112), yrange = c(29,44))
plot(x = fit, type = 'time')
plot(x = fit, type = 'space')
plot(x = fit, type = 'magnitude')
plot(x = fit, type = 'background')
}
