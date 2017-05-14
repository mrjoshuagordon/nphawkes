#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Computes the triggering function for a marked spatial temporal hawkes point process with a homonogenous and stationary background rate.
//'
//' @useDynLib nphawkes
//' @param g A vector of length n(n-1)/2, holding the triggering probabilities for time
//' @param f A vector of length n(n-1)/2, holding the triggering probabilities for distance
//' @param kp A vector of length n(n-1)/2, holding the triggering probabilities for mark productivity
//' @param mu A single numeric of length 1, holding the background rate
//' @param n A single integer indicating the number of events in the catalog.
//' @return pbNew A vector of background probabilities
//' @return ptNew A vector of triggering probabilities
//' @export
// [[Rcpp::export]]
List ComputeTriggeringMarkedSpatialTemporal(NumericVector g, NumericVector f, NumericVector kp, double mu, int n) {
  int n2 = n*(n-1)/2;
  NumericVector pbNew(n);
  NumericVector ptNew(n2);
  
  int ind1 = 0, ind2 = 0;
  for(int i = 0; i < n; i++) {
    double Z = 0;
    for(int j = 0; j < i; j++) {
      Z += kp[ind1]*g[ind1]*f[ind1];
      ind1++;
    }
    for(int j=0; j < i; j++) {
      ptNew[ind2] = (kp[ind2]*g[ind2]*f[ind2]) / (Z+mu);
      ind2++;
    }
    pbNew[i] = mu / (Z+mu);
  }    
  return List::create(Named("pbNew") =  pbNew,
                      Named("ptNew") = ptNew);
}

