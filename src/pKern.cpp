#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Calculates a probability weighted normal kernel
//'
//' @useDynLib nphawkes
//' @param prob A single value on which to calculate the kernel 
//' @param x A single number holding the longitude of interest
//' @param y A single number holding the latitude of interest
//' @param bwd A single number holding the bandwith used in the kernel
//' @return value A single value of the kernel density at the point of interest
//' @export
// [[Rcpp::export]]
double pKern(double prob, double x, double y, double bwd) {
  double value = 0;
  value = prob * (1.0 / (2.0*PI*bwd*bwd)) * std::exp(-1.0 * (x*x + y*y) / (2.0*bwd*bwd));
  return(value);
}