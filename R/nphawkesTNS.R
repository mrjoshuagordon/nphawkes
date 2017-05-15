#' Fits a Temporal Nonparametric Hawkes Point Process Model with a non stationary background rate
#'
#' @param data A list or data frame with the times of interest
#' @param nbins_t The number of bins on which to estimate the  Hawkes Point Process Model
#' @param nbins_mu The number of bins on which to estimate the background rate of the Hawkes Point Process Model
#' @param bw The background smoothing bandwidth
#' @param num_iter The maximum number of iterations to run
#' @param eps The convergence criteria
#' @param verbose A logicial to control the amount of information printed to the console during estimation
#' @param warn A logical to control the size of the dataset
#'
#' @return out An object containing the input data and fitted model
#'
#' @examples
#' data(HawkesSim1)
#' newdata <- nphData(data = HawkesSim1, time_var = 't', x_var = 'lon', y_var = 'lat',mag = 'hm')
#' fit <- nphawkesTNS(data = newdata, nbins_t = 100, nbins_mu = 50)
#' str(fit)
#' @import Rcpp
#' @export


nphawkesTNS <- function(data, nbins_t = 25, nbins_mu = 10, bw = 100 , num_iter = 1000, eps = 1e-03, verbose = T, warn = T) {
  if(!('nphData' %in% class(data))) stop('please see help(nphData)')
  if('vector' %in% class(data)) data <- list(t = as.vector(data))
  if(warn == T & length(data$t) > 12000) stop('please lower number of points')

  supDist <- function (x, y) return (max (abs (x - y)))

  tot_time <- max(data$t)
  N <- length(data$t)
  tdiffs  <- ComputeTimeDifference(x = data$t)
  delta_t  <- 10^(seq(from = min(-3, log(min(tdiffs),10)-1e-6), to = log(max(tdiffs),10)+1e-06, length = nbins_t + 1))
  grid_mu <- seq(from = min(data$t)-1e-06, to = max(data$t) + 1000, length = nbins_mu + 1)
  g <- rep(0, nbins_t)   # intialize triggering function for time



  A <- list(0)  # initialize list whose elements give the indices of time differences
  B <- list(0)
  # in tdiffs over each bin in histogram estimator g

  # find all pairs of events that fill within each bin delta A
  # time
  for(i in 2:length(delta_t)) {
    A[[i-1]] <- which(tdiffs > delta_t[i-1] & tdiffs <= delta_t[i])
    if(length(A[[i-1]]) == 1) {
      if(is.na(A[[i-1]])) A[[i-1]] = 0
    }
  }

  # time
  for(i in 2:length(grid_mu)) {
    B[[i-1]] <- which(data$t > grid_mu[i-1] & data$t <= grid_mu[i])
    if(length(B[[i-1]]) == 1) {
      if(is.na(B[[i-1]])) B[[i-1]] = 0
    }
  }


  # bin indicators
  bin_ind_t <- cut(tdiffs, breaks = delta_t, labels = F)
  bin_ind_mu <- cut(data$t, breaks = grid_mu, labels = F)

  # initialize probability
  init <- initProb(n = N)
  ptNew <- init$ptNew
  pbNew <- init$pbNew

  # start convergence algorithm
  for(k in 1:num_iter) {

    ptOld <- ptNew
    #M - step
    g <- c()
    for(j in 1:nbins_t) {
      g[j] <- sum(ptOld[A[[j]]]) / (diff(delta_t)[j]* sum(ptOld))
    }

    kp <- sum(ptOld)/N

    #E-step
    newmu <- getBackgroundT(t = data$t, pbNew = pbNew, nbins_mu = nbins_mu, grid_mu = grid_mu, nn = rep(bw, length(data$t)), delta_mu = diff(grid_mu)[1], n = length(data$t))

    res <- ComputeTriggeringTemporalNS(g = g[bin_ind_t], kp = kp, mu = newmu[bin_ind_mu], n = N)
    ptNew <- res$ptNew
    pbNew <- res$pbNew

    sup <- supDist(ptNew, ptOld)
    if( sup < eps ) break;
    if(k > 2 & k %% 10 == 0) {
      if(verbose) {
        cat (
          "Iteration: ", k,
          "SupDist: ", formatC(sup, digits = 8, width = 9, format = "f"),
          "Nb: ", sum( pbNew),
          "\n")
        if(!sumProbs(ptNew,pbNew,N)){
          cat('do not sum to 1\n')
        }
      }
    }
  }

  fit <- structure(list(bkr.rate = sum(pbNew)/length(data$t), g = g, kp = kp,
         delta_t = delta_t, Nb = sum(pbNew), grid_mu = grid_mu, mu = newmu, nbins_mu = nbins_mu,
         pbNew = pbNew),
    class = 'nphawkesTNS')
  out <- fitDataPrep(fit, data)
  class(out) <- class(fit)
  return(out)
}


