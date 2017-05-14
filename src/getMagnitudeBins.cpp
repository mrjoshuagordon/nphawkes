#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//'  Determines the magnitude bins for use in the triggering function
//'
//' @useDynLib nphawkes
//' @param nbins_m The number of distance bins on which to estimate the magnitude productivity of the Hawkes Point Process Model
//' @param mags A vector of length n(n-1)/2 holding the mainshock magnitudes between events 
//' @param grid_m The grid on which the magnitude productivity is estimated
//' @export
// [[Rcpp::export]]
NumericVector getMagnitudeBins(int nbins_m, NumericVector mags, NumericVector grid_m) {
  int n = mags.size();
  NumericVector out(n);
  int bin = 1;
  for(int l = 0; l < nbins_m; l++){
    std::vector<int> v;
    for(int i=0; i < mags.size(); i++) {
      if((mags[i] > grid_m[l]) && (mags[i] <= grid_m[l+1])){
        out[i] = bin;
      }
    }
    bin = bin + 1;
  }
  return(out);
}