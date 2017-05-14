#' Earthquake catalog from Jan 1, 1990 to June 9, 2016 
#'
#' @docType data
#'
#' @usage data(catalog)
#'
#' @format An object of class list.
#'
#' @keywords datasets
#'
#' @examples
#' data(catalog)
#' with(catalog[catalog$Magnitude > 4.0,], plot(Longitude, Latitude))
"catalog"