#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Kernel smoothing for nonstationary temporal background rate
//'
//' @useDynLib nphawkes
//' @param prob A single value on which to calculate the kernel 
//' @param t A single number holding the time of interest
//' @param bwd A single number holding the bandwith used in the kernel
//' @export 
// [[Rcpp::export]]
double pKernT(double prob, double t, double bwd) {
  double value = 0;
  value = prob * (1.0 / (2.0*PI*bwd*bwd)) * std::exp(-1.0 * (t*t) / (2.0*bwd*bwd));
  return(value);
}