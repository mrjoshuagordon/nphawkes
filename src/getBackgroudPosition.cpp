#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Computes the grid locations of each event for the background rate 
//'
//' @useDynLib nphawkes
//' @param nbins_x A single number holding the number of longitudes bins for the background rate
//' @param nbins_y A single number holding the number of latitude bins for the background rate
//' @param x A vector of length n, holding the longitudes of interest
//' @param y A vector of length n, holding the latitude of interest
//' @param grid_x A vector holding the grid positions for the longitudes bins of the background rate
//' @param grid_y A vector holding the grid positions for the latitude bins of the background rate
//' @param n A single integer indicating the number of events in the catalog
//' @return D A vector of length nbins_x  * nbins_y holding vectors with event indices if they fall within the grid cell 
//' @export
// [[Rcpp::export]]
List getBackgroudPosition(int nbins_x, int nbins_y, NumericVector x, NumericVector y, NumericVector grid_x, NumericVector grid_y,  int n){
std::vector<std::vector<int> > D;
  for(int k=0; k < nbins_x; k++) {
    for(int l=0; l < nbins_y; l++) {
      std::vector<int> v;
      for(int i=0; i < n; i++) {
        if((x[i] > grid_x[k]) && (x[i] <= grid_x[k+1]) && (y[i] > grid_y[l]) && (y[i] <= grid_y[l+1])){
          v.push_back(i);
        }
      }
      if(v.empty()){
        v.push_back(-1);
      }
      D.push_back(v);
      v.clear();
    }
  }
  return wrap(D);
}