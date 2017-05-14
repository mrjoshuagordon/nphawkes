#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//'  Determines the time bins for use in the triggering function
//'
//' @useDynLib nphawkes
//' @param nbins_t The number of time bins on which to estimate the temporal triggering of the Hawkes Point Process Model
//' @param time A vector of length n(n-1)/2 holding the time between events 
//' @param grid_t The grid on which the temporal triggering distribution is estimated
//' @export
// [[Rcpp::export]]
NumericVector getTimeBins(int nbins_t, NumericVector time, NumericVector grid_t) {
  int n = time.size();
  NumericVector out(n);
  int bin = 1;
  for(int l = 0; l < nbins_t; l++){
    std::vector<int> v;
    for(int i=0; i < time.size(); i++) {
      if((time[i] > grid_t[l]) && (time[i] <= grid_t[l+1])){
        out[i] = bin;
      }
    }
    bin = bin + 1;
  }
  return(out);
}