#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Compute the distance between events.
//'
//' @useDynLib nphawkes
//' @param x A vector of length n, holding the longitudes of interest
//' @param y A vector of length n, holding the latitudes of interest
//' @param n A single integer indicating the number of events  
//' @return dist A vector of length n(n-1)/2, holding the distance between events
//' @export
// [[Rcpp::export]]
NumericVector ComputeDistance(NumericVector x, NumericVector y, int n) {
  int n2 = n*(n-1)/2;
  NumericVector dist(n2);
  int ind = 0;
  double r;
  
  for(int i=0; i < n; i++) {
    for(int j=0; j < i; j++) {
      r = sqrt(pow(x[i] - x[j],2) + pow(y[i] - y[j],2));
      if(r > 1.0e-6) {
        dist[ind] = r; 
      } else {
        dist[ind] = (1.0e-6);
      }
      ind++;
    }
  }
  return dist;
}	