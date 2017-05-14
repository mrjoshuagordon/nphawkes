#' Function to plot the magnitude productivity, spatial, temporal, and background components of the Hawkes model
#' @param x An object of class nphawkesMSTH
#' @param type A string indicating the type of plot to generate
#' @param print A logical indicating whether the plot should be printed or returned
#' @param ... Other parameters passed in
#'
#' @return p A ggplot2 plot
#' @examples
#' data(HawkesSim1)
#' newdata <- nphData(data = HawkesSim1, time_var = 't', x_var = 'lon', y_var = 'lat',mag = 'hm')
#' fit <- nphawkesTNS(data = newdata, nbins_t = 100, nbins_mu = 50)
#' plot(x = fit, type = 'time')
#' plot(x = fit, type = 'background')
#' plot(x = fit, type = 'background', smooth = TRUE)
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @export

plot.nphawkesTNS <- function(x, type = 'time', smooth = FALSE, print = FALSE, ...){
  # S3 methods!
  data <- x
  if(class(data) != "nphawkesTNS") stop('Fit must be of class nphawkesTNS')
  if(tolower(substring(type,1,1)) == 't'){
    g_p <- ggplot(data$est_time[data$est_time$est>0,]) + geom_segment(aes(x = t_lb, xend = t_ub, y = est, yend = est),na.rm=TRUE) + theme_bw()
    g_p <- g_p + labs(x = 'Time', y =  expression(hat(g)(t)))
    p <- g_p + scale_x_log10() + scale_y_log10(limits = c(min(data$est_time$t_lb), max(data$est_time$t_ub)))
  } else{
    g_p <- ggplot(data$est_mu) + geom_segment(aes(x = grid_mu_lb, xend = grid_mu_ub, y = est, yend = est),na.rm=TRUE) + theme_bw()
    g_p <- g_p + labs(x = 'Time', y =  expression(hat(mu)(t)))
    if(smooth == TRUE) g_p <- g_p + geom_smooth(aes(x = (grid_mu_lb + grid_mu_ub)/2, y = est), method = 'loess', se = FALSE)
    p <- g_p
  }
  if(!print){
    suppressWarnings(p)
  } else{
    suppressWarnings(print(p))
  }
}

