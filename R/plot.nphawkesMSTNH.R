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
#' fit <- nphawkesMSTNH(data = data, np = 100, eps = 1e-2, xrange = c(-127, -112), yrange = c(29,44))
#' plot(x = fit, type = 'time')
#' plot(x = fit, type = 'space')
#' plot(x = fit, type = 'magnitude')
#' plot(x = fit, type = 'background')
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @export

plot.nphawkesMSTNH <- function(x, type = 'time', print = FALSE, ...){
  # S3 methods!
  data <- x
  if(class(data) != "nphawkesMSTNH") stop('Fit must be of class nphawkesMSTNH')
  if(tolower(substring(type,1,1)) == 'm'){
    kappa_p <- ggplot(data$est_kappa[data$est_kappa$est>0,]) + geom_segment(aes(x = m_lb, xend = m_ub, y = est, yend = est),na.rm=TRUE) + theme_bw()
    p <- kappa_p + labs(x = 'Magnitude', y =  expression(hat(kappa)(m)))
  } else if(tolower(substring(type,1,1)) == 't'){
    g_p <- ggplot(data$est_time[data$est_time$est>0,]) + geom_segment(aes(x = t_lb, xend = t_ub, y = est, yend = est),na.rm=TRUE) + theme_bw()
    g_p <- g_p + labs(x = 'Time', y =  expression(hat(g)(t)))
    p <- g_p + scale_x_log10() + scale_y_log10(limits = c(min(data$est_time$t_lb), max(data$est_time$t_ub)))
  } else if(tolower(substring(type,1,1)) == 's'){
      f_p <- ggplot(data$est_space[data$est_space$est>0,]) + geom_segment(aes(x = r_lb, xend = r_ub, y = est, yend = est),na.rm=TRUE) + theme_bw()
      f_p <- f_p + labs(x = 'Distance', y =  expression(hat(g)(t)))
      p <- f_p + scale_x_log10(limits = c(1e-5, max(data$est_space$r_ub))) + scale_y_log10(limits = c(1e-5, max(data$est_space$est)))
  } else{
    lon_grid <- seq(data$xmin, data$xmin + data$x_window, length = data$nbins_mu + 1)
    lat_grid <- seq(data$ymin, data$ymin + data$y_window, length = data$nbins_mu + 1)
    the_grid1 <- expand.grid(minlon = lon_grid[-length(lon_grid)], maxlat = lat_grid[-1])
    the_grid2 <- expand.grid(maxlon = lon_grid[-1], minlat = lat_grid[-length(lat_grid)])
    bkrd_df <-data.frame(est = data$est_mu, the_grid1, the_grid2)
    bkrd_df <- bkrd_df %>% mutate(midlon = (maxlon + minlon)/2, midlat = (maxlat + minlat)/2)

    p <- ggplot(data = bkrd_df, mapping = aes(x = midlon, y = midlat)) +
      theme_bw() +
      geom_tile(aes(fill = est)) +
      scale_fill_gradientn(
        colours=c( "blue","green","yellow", "red", "purple"),
        limits = c(0, max(bkrd_df$est)))
    labs(title = "Background Rate", x = "Longitude", y = "Latitude") +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "grey70"))
  }
  if(!print){
    suppressWarnings(p)
  } else{
    suppressWarnings(print(p))
  }
}

