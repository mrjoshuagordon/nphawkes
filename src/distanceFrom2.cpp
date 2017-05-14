#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Computes the distance from one events to all other events and sort them is ascending order
//'
//' @useDynLib nphawkes
//' @param x0 A single double indicating the position of the origin event
//' @param y0 A single double indicating the position of the origin event
//' @param x1 A vector of length n, holding the longitudes of interest
//' @param y1 A vector of length n, holding the latitude of interest
//' @return dist A vector of distances from the origin event
//' @export
// [[Rcpp::export]]
NumericVector distanceFrom2(double x0, double y0, NumericVector x1, NumericVector y1){
  int N = x1.size();
  NumericVector dist(N);
  int ind = 0;  
  for(int i=0; i < N; i++) {
      dist[ind] = sqrt(pow(x0 - x1[i],2) + pow(y0 - y1[i],2));
      ind++;
  }
  return dist;
}