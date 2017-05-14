#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//'  Determines the distance bins for use in the triggering function
//'
//' @useDynLib nphawkes
//' @param nbins_r The number of distance bins on which to estimate the spatial triggering of the Hawkes Point Process Model
//' @param dist A vector of length n(n-1)/2 holding the distance between events 
//' @param grid_r The grid on which the spatial triggering distribution is estimated
//' @export
// [[Rcpp::export]]
NumericVector getDistanceBins(int nbins_r, NumericVector dist, NumericVector grid_r) {
  int n = dist.size();
  NumericVector out(n);
  int bin = 1;
  for(int l = 0; l < nbins_r; l++){
    std::vector<int> v;
    for(int i=0; i < dist.size(); i++) {
      if((dist[i] > grid_r[l]) && (dist[i] <= grid_r[l+1])){
        out[i] = bin;
      }
    }
    bin = bin + 1;
  }
  return(out);
}