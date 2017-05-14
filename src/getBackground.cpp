#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

double sumVector(Rcpp::NumericVector v);
double pKern(double prob, double x, double y, double bwd);

//' Computes the background probability associated with each point in the catalog
//'
//' @useDynLib nphawkes
//' @param x A vector of length n, holding the longitudes of interest
//' @param y A vector of length n, holding the latitude of interest
//' @param t A Single number, holding the maximum time of interest
//' @param pbNew A vector of length n, holding the background probabilities
//' @param nn An integer for the n-th nearest neighbor to use in background smoothing
//' @param nbins_x A single number holding the number of longitudes bins for the background rate
//' @param nbins_y A single number holding the number of latitude bins for the background rate
//' @param grid_x A vector holding the grid positions for the longitudes bins of the background rate
//' @param grid_y A vector holding the grid positions for the latitude bins of the background rate
//' @param delta_x A single number holding the bin width longitudes bins for the background rate
//' @param delta_y A single number holding the bin height of latitude bins for the background rate
//' @param n A single integer indicating the number of events in the catalog
//' @return mu A vector of length n holding the new background probabilities 
//' @export
// [[Rcpp::export]]
NumericVector getBackground(NumericVector x, NumericVector y, double t, NumericVector pbNew, NumericVector nn, int nbins_x, int nbins_y, NumericVector grid_x, NumericVector grid_y, double delta_x, double delta_y, int n){
  int ind = 0;
  int nbins_mu = nbins_x * nbins_y;
  double sum = 0, xmid = 0, ymid = 0;
  NumericVector mu(nbins_mu);
  for(int i=0; i< nbins_x; i++) {
    xmid = (grid_x[i+1] + grid_x[i]) / 2.0;
    for(int j=0; j<nbins_y; j++) {
      ymid = (grid_y[j+1] + grid_y[j]) / 2.0;
      sum = 0;
      for(int k=0; k < n; k++) {
          sum += pKern(pbNew[k], xmid - x[k], ymid - y[k], nn[k]);
      }
      mu[ind] = sum;
      ind++;
    }
  }
  
  double Z=0, gamma = 0;
  gamma = sumVector(pbNew) / t;
  for(int i=0; i<nbins_mu; i++) {
    Z += mu[i] * delta_x * delta_y; //Reimann sum
  }
  for(int i=0; i<nbins_mu; i++) {
    mu[i] = gamma * mu[i] / Z;
  }
  return mu;
}


