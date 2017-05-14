#' Prepares nphawkes model estimation and nphawkes data for nphawkes forecasting
#'
#' @param fit An object of class nphawkesMSTNH, nphawkesMSTNA, or nphawkesMSTNAC
#' @param data An object of class nphData
#' @param forecast_date The date for which to generate a daily forecast
#' @param catalog_start The first date of the catalog using in model fitting used to calculate the tot_time parameter used in the thinning of background events
#' @return data A list of outputs list(est_mu, nbins_mu, x_window, y_window, Nb, xmin, ymin, tot_time, t_obs, x_obs, y_obs, m_obs)
#'
#' @examples
#' data(catalog)
#' data <- nphData(data = catalog[catalog$Magnitude > 4.0,], time_var = 'tdiff', x_var = 'Longitude', y_var = 'Latitude', mag = 'Magnitude')
#' fit <- nphawkesMSTNH(data = data, np = 100, eps = 1e-2, xrange = range(data$lon), yrange = range(data$lat))
#' myForecastData <- forecastDataPrep(fit = fit, data = data, forecast_date = as.Date(max(data$DateTime)) + 1, catalog_start = min(data$DateTime))
#' str(myForecastData)
#' @export
forecastDataPrep  <- function(fit, data, forecast_date, catalog_start = '1985-01-01'){
  mind <- strptime(catalog_start, format = '%Y-%m-%d', tz = 'UTC')
  forecast_date <- strptime(forecast_date, format = '%Y-%m-%d', tz = 'UTC')
  out            <- list()
  out$catalog_start <- as.Date(mind)
  out$est_kappa  <- data.frame(m_lb = fit$delta_m[1:(length(fit$delta_m))-1], m_ub = fit$delta_m[-1], est = fit$kp)
  out$est_time   <- data.frame(t_lb = fit$delta_t[1:(length(fit$delta_t))-1], t_ub = fit$delta_t[-1], est = fit$g)
  if(class(fit) == 'nphawkesMSTNH'){
    out$est_space  <- data.frame(r_lb = fit$delta_r[1:(length(fit$delta_r))-1], r_ub = fit$delta_r[-1], est = fit$f)
    out$tot_time   <- as.numeric(as.Date(forecast_date) - as.Date(mind))
  } else if(class(fit) == 'nphawkesMSTNHA'){
    out$est_space  <- data.frame(r_lb = rep(fit$delta_r[1:(length(fit$delta_r))-1], times = length(unique(fit$delta_a)) - 1),
                                 r_ub = rep(fit$delta_r[-1], times = length(unique(fit$delta_a)) - 1),
                                 a_lb = rep(fit$delta_a[1:(length(fit$delta_a))-1], each = length(unique(fit$delta_r)) - 1),
                                 a_ub = rep(fit$delta_a[-1], each = length(unique(fit$delta_r)) - 1),
                                 m_lb = fit$mag_cut,
                                 m_ub = max(data$hm),
                                 est = fit$f)
    out$mag_cut    <- fit$mag_cut
    keep           <- data$hm >=  fit$mag_cut & data$DateTime < as.POSIXct(forecast_date)
    out$phi        <- fit$phi[keep]
  } else if(class(fit) == 'nphawkesMSTNHAC'){
    f_length <- length(fit$f)
    out$est_space  <- data.frame(matrix(0, nrow = f_length, ncol = 7))
    names(out$est_space) <- c("r_lb", "r_ub", "a_lb", "a_ub", "m_lb", "m_ub", "est")
    j <- 1
    for(ita in 1:(length(fit$delta_a) - 1)){
      for(itr in 1:(length(fit$delta_r)-1)){
        for(itm in 1:(length(fit$delta_m2)-1)){
          out$est_space$a_lb[j] <- fit$delta_a[ita]
          out$est_space$a_ub[j] <- fit$delta_a[ita + 1]
          out$est_space$r_lb[j] <- fit$delta_r[itr]
          out$est_space$r_ub[j] <- fit$delta_r[itr + 1]
          out$est_space$m_lb[j] <- fit$delta_m2[itm]
          out$est_space$m_ub[j] <- fit$delta_m2[itm + 1]
          j <- j + 1
        }
      }
    }
    out$est_space$est <- fit$f

    out$mag_cut    <- fit$mag_cut
    keep           <- data$hm >=  fit$mag_cut & data$DateTime < as.POSIXct(forecast_date)
    keep2          <- keep[ data$hm >=  fit$mag_cut ]
    out$phi        <- fit$phi[keep2 ]
    out$tot_time   <- as.numeric(as.Date(forecast_date) - as.Date(mind))
  }

  out$est_mu     <- fit$mu
  out$nbins_mu   <- fit$nbins_x
  out$xrange     <- fit$xrange
  out$yrange     <- fit$yrange
  out$x_window   <- diff(fit$xrange)
  out$y_window   <- diff(fit$yrange)
  out$Nb         <- sum(fit$pbNew)
  out$xmin       <- fit$xrange[1]
  out$ymin       <- fit$yrange[1]
  keep           <- data$t < out$tot_time
  out$t_obs      <- data$t[keep]
  out$x_obs      <- data$lon[keep]
  out$y_obs      <- data$lat[keep]
  out$m_obs      <- data$hm[keep]
  class(out) <- 'forecastDataPrep'
  return(out)
}
