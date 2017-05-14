#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;
NumericVector distanceFrom(NumericVector x1, NumericVector y1, int j);
//' Determines the distance of the specified nearest neighbor
//'
//' @useDynLib nphawkes
//' @param x A vector of length n, holding the longitudes of interest
//' @param y A vector of length n, holding the latitude of interest
//' @param np A single integer indicating the nth nearest neighbor
//' @param n A single integer indicating the  number of events
//' @return nn A vector of nearest neighbor distances
//' @export
// [[Rcpp::export]]
NumericVector getNN(NumericVector x, NumericVector y, int np, int n) {
  NumericVector nn(n); 
  NumericVector v; 
  for(int i = 0; i < n; i++) {
    v = distanceFrom(x,y,i);
    nn[i] = std::max(v[np-1] + 1e-08,0.02);
  }  
  return nn;
}