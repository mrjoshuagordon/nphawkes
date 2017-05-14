#' Fits a Marked Spatial Temporal Nonparametric Hawkes Point Process Model with a nonhomogenous and stationary background rate
#'
#' @param data A list or data frame with the times, locations, and magnitudes of interest
#' @param nbins_m The number of bins on which to estimate the magnitude productivity function for Hawkes Point Process Model
#' @param nbins_t The number of bins on which to estimate the spatial triggering of the Hawkes Point Process Model
#' @param nbins_r The number of bins on which to estimate the temporal triggering of the Hawkes Point Process Model
#' @param np The nearest neighbor used for bandwidth selection
#' @param num_iter The maximum number of iterations to run
#' @param nbins_x The number of x bins on which to estimate the inhomogenous background rate
#' @param nbins_y The number of y bins on which to estimate the inhomogenous background rate
#' @param xrange The x range on which estimate the spatial triggering function
#' @param yrange The y range on which estimate the spatial triggering function
#' @param eps The convergence criteria
#' @param verbose A logicial to control the amount of information printed to the console during estimation
#' @param warn A logical to control the size of the dataset
#'
#' @return out An object containing the input data and fitted model
#'
#' @examples
#' data(ETASsim)
#' newdata <- nphData(data = ETASsim, time_var = 't', x_var = 'lon', y_var = 'lat', mag = 'hm')
#' res <- nphawkesMSTNH(data = newdata, xrange = c(0,4), yrange = c(0,6), eps = 1e-2)
#'
#' @export
nphawkesMSTNH <- function(data, nbins_m = 8, nbins_t = 25, nbins_r = 25, np = 50, num_iter = 1000,
                          nbins_x = 100, nbins_y = 100, xrange = NULL, yrange = NULL, eps = 1e-03, verbose = T, warn = T) {
  supDist <- function (x, y) return (max (abs (x - y)))
  if(!('nphData' %in% class(data))) stop('please see help(nphData)')
  if(warn == T & length(data$t) > 12000) stop('please lower number of points')
  if(is.null(xrange)) xrange <- range(data$lon)
  if(is.null(yrange)) yrange <- range(data$lat)
  tot_time <- max(data$t)
  N <- length(data$t)
  tdiffs    <- ComputeTimeDifference(x = data$t)
  rdiffs    <- ComputeDistance(x = data$lon, y = data$lat, n = N)
  mags      <- getMags(m = data$hm, n = N)
  delta_m  <- seq(floor(min(data$hm)), max(data$hm) + .01, length = nbins_m + 1)
  delta_t  <- 10^(seq(from = min(-3, log(min(tdiffs),10)-1e-6), to = log(max(tdiffs),10)+1e-06, length = nbins_t + 1))
  delta_r  <- 10^(seq(from = floor(min(log(rdiffs,10))-1e-6), to = log(sqrt(diff(xrange)^2 + diff(yrange)^2) + .5 ,10), length = nbins_r + 1))
  delta_x  <- diff(xrange)
  delta_y  <- diff(yrange)
  grid_x   <- seq(min(xrange), max(xrange), length = nbins_x + 1)
  grid_y   <- seq(min(yrange), max(yrange), length = nbins_y + 1)
  nn       <- getNN(x = data$lon, y = data$lat, np = np, n = N)

  # bin indicators
  bin_ind_t   <- getTimeBins(nbins_t = nbins_t, time = tdiffs, grid_t = delta_t)
  bin_ind_r   <- getDistanceBins(nbins_r = nbins_r, dist = rdiffs, grid_r = delta_r)
  bin_ind_m   <- getMagnitudeBins(nbins_m = nbins_m, mags = mags, grid_m = delta_m)
  Nm          <- as.numeric(table(cut(data$hm, breaks = delta_m)))
  D  <- getBackgroudPosition(nbins_x = nbins_x, nbins_y = nbins_y, x = data$lon, y = data$lat,
                             grid_x = grid_x, grid_y = grid_y, n = N)

  kp <- rep(0, nbins_m)
  g  <- rep(0, nbins_t)   # intialize triggering function for time
  f  <- rep(0, nbins_r)   # intialize triggering function for distance

  A <- list(0)  # initialize list whose elements give the indices of time differences
  B <- list(0)
  C <- list(0)

  # find all pairs of events that fill within each bin delta A
  # time
  for(i in 1:(nbins_t)){
    A[[i]] <- which(bin_ind_t == i)
    if(length(A[[i]]) == 1) {
      if(is.na(A[[i]])) A[[i]] <- 0
    }
  }

  # distance
  for(i in 1:(nbins_r)){
    B[[i]] <- which(bin_ind_r == i)
    if(length(B[[i]]) == 1) {
      if(is.na(B[[i]])) B[[i]] <- 0
    }
  }

  # magnitude
  for(i in 1:(nbins_m)){
    C[[i]] <- which(bin_ind_m == i)
    if(length(C[[i]]) == 1) {
      if(is.na(C[[i]])) C[[i]] <- 0
    }
  }


  # initialize probability
  init  <- initProb(n = N)
  ptNew <- init$ptNew
  pbNew <- init$pbNew

  if(verbose) cat('start convergence algorithm...\n')
  # start convergence algorithm
  for(k in 1:num_iter) {

    ptOld <- ptNew
    #M - step

    g <- c()
    for(j in 1:nbins_t) {
      g[j] <- sum(ptOld[A[[j]]]) / (diff(delta_t)[j]* sum(ptOld))
    }

    f <- c()
    for(j in 1:nbins_r) {
      f[j] <- sum(ptOld[B[[j]]]) / (diff(delta_r)[j]* sum(ptOld))
    }

    kp <- c()
    for(j in 1:nbins_m) {
      if(Nm[j] == 0){
        kp[j] <- 0
      } else{
        kp[j] <- sum(ptOld[C[[j]]]) / Nm[j]
      }
    }

    #E-step
    # figure out where each mu goes using a 2-d cut
    mu <- getBackground(x = data$lon, y = data$lat, t = max(data$t),
                       delta_x = diff(grid_x)[1], delta_y = diff(grid_y)[1], pbNew = pbNew,
                       nn = nn, nbins_x = nbins_x, nbins_y = nbins_y,
                       grid_x = grid_x, grid_y = grid_y, n = N)


    #rep(mu,bin_ind_mu)
    pbNew <- replaceMu(mu = mu, D = D, n = N)

    res <- ComputeTriggeringMarkedSpatialTemporalNH(g = g[bin_ind_t], f = f[bin_ind_r]/(2*pi*rdiffs), kp = kp[bin_ind_m], mu = pbNew , n = N)
    ptNew <- res$ptNew
    pbNew <- res$pbNew

    sup <- supDist(ptNew, ptOld)
    if( sup < eps ) break; #return(list(mu = mu, g = g, f = f, delta_t = delta_t))
    if(k > 0 & k %% 1 == 0) {
      if(verbose) {
        cat (
          "Iteration: ", k,
          "SupDist: ", formatC(sup, digits = 8, width = 12, format = "f"),
          "Nb: ", sum(pbNew),
          "\n")
        if(!sumProbs(ptNew,pbNew,N)){
          cat('do not sum to 1\n')
        }
      }
    }
  }

  fit <- structure(list(mu = mu, g = g, f = f, kp = kp, delta_t = delta_t,
                        delta_r = delta_r, delta_m = delta_m,
                        nbins_x = nbins_x, nbins_y = nbins_y,
                        tot_time = tot_time, xrange = xrange,
                        yrange = yrange, np = np, pbNew = pbNew), class = 'nphawkesMSTNH')
  out <- fitDataPrep(fit, data)
  class(out) <- class(fit)
  return(out)
}


