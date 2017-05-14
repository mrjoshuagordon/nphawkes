#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Compute the time differences between events.
//'
//' @useDynLib nphawkes
//' @param x A vector of length n, holding the times of interest
//' @return tdiff A vector of length n(n-1)/2, holding the time differences
//' @export
// [[Rcpp::export]]
NumericVector ComputeTimeDifference(NumericVector x) {
  int n = x.size();
  int n2 = n*(n-1)/2;
  NumericVector tdiff(n2);

  int ind = 0;
  for(int i=0; i < n; i++) {
    for(int j=0; j < i; j++) {
      if(x[i] - x[j] > 1.0e-3){ 
      tdiff[ind] =  x[i] - x[j];
      } else{
         tdiff[ind] = 1.0e-3;
      }
      ind++;
    }
  }
  return tdiff;
}	