#' Function to plot the magnitude productivity, spatial, temporal, and background components of the Hawkes model
#' @param x An object of class nphawkesMSTH
#' @param type A string indicating the type of plot to generate
#' @param print A logical indicating whether the plot should be printed or returned
#' @param ... Other parameters passed in
#'
#' @return p A ggplot2 plot
#' @examples
#' data(catalog)
#' data <- nphData(data = catalog[catalog$Magnitude > 4.0,], time_var = 'tdiff', x_var = 'Longitude', y_var = 'Latitude', mag = 'Magnitude')
#' fit <- nphawkesT(data = data)
#' plot(x = fit, type = 'time')
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @export

plot.nphawkesT <- function(x, print = FALSE, ...){
  # S3 methods!
  data <- x
  if(class(data) != "nphawkesT") stop('Fit must be of class nphawkesT')
  g_p <- ggplot(data$est_time[data$est_time$est>0,]) + geom_segment(aes(x = t_lb, xend = t_ub, y = est, yend = est),na.rm=TRUE) + theme_bw()
  g_p <- g_p + labs(x = 'Time', y =  expression(hat(g)(t)))
  p <- g_p + scale_x_log10() + scale_y_log10(limits = c(min(data$est_time$t_lb), max(data$est_time$t_ub)))

  if(!print){
    suppressWarnings(p)
  } else{
    suppressWarnings(print(p))
  }
}

