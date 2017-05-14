#' Prepares data for nphawkes estimation
#'
#' @param data A list or data frame with event information
#' @param time_var A character string representing the name of the time variable in the current data set
#' @param x_var A character string representing the name of the x variable (lon) in the current data set
#' @param y_var A character string representing the name of the y variable (lat) in the current data set
#' @param mag A character string represting the name of the magnitude variable in the current data set
#'
#' @return data A dataset with modified names
#'
#' @examples
#' mydata <- data.frame(times = runif(100), lon = runif(100, 0, 4), lat = runif(100, 0, 6), magnitudes = rexp(100))
#' newdata <- nphData(data = mydata, time_var = 'times', x_var = 'lon', y_var = 'lat', mag = 'magnitudes')
#' res <- nphawkesMSTNH(data = newdata)
#' @export
nphData <- function(data = NULL, time_var = NULL, x_var = NULL, y_var = NULL, mag = NULL){
  data <- data[c( time_var, x_var, y_var,mag)]
  if(any(is.na(data))) stop('Please remove all NAs')
  if(is.null(data)) stop('Please include a dataset')
  if(all(is.null(c(time_var, x_var, y_var, mag)))) stop('Please specify a time_var')
  names(data)[which(names(data) == time_var)] <- 't'
  if(class(data$t) != 'numeric') stop('time_var should be specified t_i - t_o')
  if(!is.null(x_var) & !is.null(y_var)){
    names(data)[which(names(data) == x_var)] <- 'lon'
    names(data)[which(names(data) == y_var)] <- 'lat'
  } else{
    cat('x_var and/or y_var not provided and will not work in nphawkesMSTNH estimation','\n')
  }
  if(!is.null(mag)){
    names(data)[which(names(data) == mag)] <- 'hm'
  } else{
    cat('mag not provided and will not work in nphawkesMSTNH estimation','\n')
  }
  if(class(data) != 'data.frame') data <- data.frame(data)
  if(length(c( time_var, x_var, y_var,mag)) > 1){
    data <- data[order(data$t),]
    class(data) <- c('nphData','data.frame')
  } else{
    data <- sort(data$t)
    class(data) <- c('vector','nphData')
  }
  return(data)
}

