#' Prepares nphawkes model estimation and nphawkes data for nphawkes forecasting
#'
#' @param fit An object of class nphawkesMSTNH, nphawkesMSTNA, or nphawkesMSTNAC
#' @param data An object of class nphData
#' @export
#'
fitDataPrep  <- function(fit, data){
  if('nphawkesMSTNH' %in% class(fit)){
    out            <- list()
    out$est_kappa  <- data.frame(m_lb = fit$delta_m[1:(length(fit$delta_m))-1], m_ub = fit$delta_m[-1], est = fit$kp)
    out$est_time   <- data.frame(t_lb = fit$delta_t[1:(length(fit$delta_t))-1], t_ub = fit$delta_t[-1], est = fit$g)
    out$est_space  <- data.frame(r_lb = fit$delta_r[1:(length(fit$delta_r))-1], r_ub = fit$delta_r[-1], est = fit$f)
    out$est_mu     <- fit$mu
    out$nbins_mu   <- fit$nbins_x
    out$xrange     <- fit$xrange
    out$yrange     <- fit$yrange
    out$x_window   <- diff(fit$xrange)
    out$y_window   <- diff(fit$yrange)
    out$Nb         <- sum(fit$pbNew)
    out$xmin       <- fit$xrange[1]
    out$ymin       <- fit$yrange[1]
    out$t_obs      <- data$t
    out$x_obs      <- data$lon
    out$y_obs      <- data$lat
    out$m_obs      <- data$hm
    return(out)
  } else if('nphawkesT' %in% class(fit)){
    out            <- list()
    out$est_time   <- data.frame(t_lb = fit$delta_t[1:(length(fit$delta_t))-1], t_ub = fit$delta_t[-1], est = fit$g)
    out$est_mu     <- fit$mu
    out$nbins_mu   <- 1
    out$trange   <- range(fit$delta_t)
    out$Nb         <- sum(fit$pbNew)
    out$t_obs      <- data$t
    return(out)
  } else if('nphawkesTNS' %in% class(fit)){
    out            <- list()
    out$est_kappa  <- fit$kp
    out$est_time   <- data.frame(t_lb = fit$delta_t[1:(length(fit$delta_t))-1], t_ub = fit$delta_t[-1], est = fit$g)
    out$est_mu     <- data.frame(grid_mu_lb = fit$grid_mu[1:(length(fit$grid_mu))-1], grid_mu_ub = fit$grid_mu[-1], est = fit$mu)
    out$nbins_mu   <- fit$nbins_mu
    out$trange     <- range(fit$delta_t)
    out$Nb         <- sum(fit$pbNew)
    out$t_obs      <- data$t
    return(out)
  }
}
