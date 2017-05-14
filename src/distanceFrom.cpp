#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Computes the distance from one events to all other events and sort them is ascending order
//'
//' @useDynLib nphawkes
//' @param x1 A vector of length n, holding the longitudes of interest
//' @param y1 A vector of length n, holding the latitude of interest
//' @param j A single integer indicating the position of the origin event
//' @return dist1 A sorted vector of distances from the origin event
//' @export
// [[Rcpp::export]]
NumericVector distanceFrom(NumericVector x1, NumericVector y1, int j){
  int n = x1.size();
  NumericVector dist(n-1);
  int ind = 0;  
  for(int i = 0; i < n; i++) {
    if(i != j){
      dist[ind] = sqrt(pow(x1[j] - x1[i],2) + pow(y1[j] - y1[i],2));
      ind++;
    }
  }
  NumericVector dist1 = clone(dist);
  std::sort(dist1.begin(), dist1.end());
  return dist1;
}