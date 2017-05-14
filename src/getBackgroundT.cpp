#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

double sumVector(Rcpp::NumericVector v);
double pKernT(double prob, double t, double bwd);

//' Computes the background probability associated with each point in the catalog
//'
//' @useDynLib nphawkes
//' @param t A vector of length n, holding the times of interest
//' @param pbNew A vector of length n, holding the background probabilities
//' @param nn A single number holding the nearest neighbor bandwith used in the kernel
//' @param nbins_mu A single number holding the number of time bins for the background rate
//' @param grid_mu A vector holding the grid positions for the time  bins of the background rate
//' @param delta_mu A single number holding the bin size of the time bins for the background rate
//' @param n A single integer indicating the number of events in the catalog
//' @export
// [[Rcpp::export]]
NumericVector getBackgroundT(NumericVector t, NumericVector pbNew, NumericVector nn, int nbins_mu, NumericVector grid_mu, double delta_mu, int n){
  int ind = 0;
  double sum = 0, mumid = 0;
  NumericVector mu(nbins_mu);
  for(int i=0; i < nbins_mu; i++) {
    mumid = (grid_mu[i+1] + grid_mu[i]) / 2.0;
    sum  = 0;
    for(int k = 0; k < n; k++) {
      sum += pKernT(pbNew[k], mumid - t[k], nn[k]);
    }
    mu[ind] = sum;
    ind++;
  }
  
  double Z=0;
  for(int i=0; i < nbins_mu; i++) {
    Z += mu[i] * delta_mu; 
  }
  for(int i=0; i < nbins_mu; i++) {
    mu[i] = sumVector(pbNew) * mu[i] / Z;
  }
  return(mu);
}