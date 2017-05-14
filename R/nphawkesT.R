#' Fits a Temporal Nonparametric Hawkes Point Process Model with a stationary background rate
#'
#' @param data A list or data frame with the times of interest
#' @param nbins_t The number of bins on which to estimate the  Hawkes Point Process Model
#' @param num_iter The maximum number of iterations to run
#' @param eps The convergence criteria
#' @param verbose A logicial to control the amount of information printed to the console during estimation
#' @param warn A logical to control the size of the dataset
#'
#' @return out An object containing the input data and fitted model
#'
#' @examples
#' data(ETASsim)
#' newdata <- nphData(data = ETASsim, time_var = 't')
#' res <- nphawkesT(data = newdata, nbins_t = 30)
#' @import Rcpp
#' @export
nphawkesT <- function(data, nbins_t = 25, num_iter = 1000, eps = 1e-03, verbose = T, warn = T) {
  if(!('nphData' %in% class(data))) stop('please see help(nphData)')
  data <- list(t = as.vector(data))
  if(warn == T & length(data$t) > 12000) stop('please lower number of points')
  supDist <- function (x, y) return (max (abs (x - y)))

  tot_time <- max(data$t)
  N <- length(data$t)
  tdiffs  <- ComputeTimeDifference(x = data$t)
  delta_t  <- 10^(seq(from = min(-3, log(min(tdiffs),10)-1e-6), to = log(max(tdiffs),10)+1e-06, length = nbins_t + 1))
  g <- rep(0, nbins_t)   # intialize triggering function for time



  A <- list(0)  # initialize list whose elements give the indices of time differences
  # in tdiffs over each bin in histogram estimator g

  # find all pairs of events that fill within each bin delta A
  # time
  for(i in 2:length(delta_t)) {
    A[[i-1]] <- which(tdiffs > delta_t[i-1] & tdiffs <= delta_t[i])
    if(length(A[[i-1]]) == 1) {
      if(is.na(A[[i-1]])) A[[i-1]] = 0
    }
  }


  # bin indicators
  bin_ind_t <- cut(tdiffs, breaks = delta_t, labels = F)

  # initialize probability
  init <- initProb(n = N)
  P_new <- init$ptNew
  mu <- init$pbNew

  # start convergence algorithm
  for(k in 1:num_iter) {

    P_old <- P_new
    #M - step
    g <- c()
    for(j in 1:nbins_t) {
      g[j] <- sum(P_old[A[[j]]]) / (diff(delta_t)[j]* sum(P_old))
    }

    #E-step
    newmu <- sum(mu)/(tot_time)

    res <- ComputeTriggeringTemporal(g = g[bin_ind_t], mu = newmu , n = N)
    P_new <- res$ptNew
    mu <- res$pbNew

    sup <- supDist(P_new, P_old)
    if( sup < eps ) break;
    if(k > 2 & k %% 10 == 0) {
      if(verbose) {
        cat (
          "Iteration: ", k,
          "SupDist: ", formatC(sup, digits = 8, width = 9, format = "f"),
          "Nb: ", sum(mu),
          "\n")
        if(!sumProbs(P_new,mu,N)){
          cat('do not sum to 1\n')
        }
      }
    }
  }

  fit <- structure(list(mu = mu, g = g, delta_t = delta_t), class = 'nphawkesT')
  out <- fitDataPrep(fit, data)
  class(out) <- class(fit)
  return(out)
}

