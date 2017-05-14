#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Initialize triggering and background probabilities 
//'
//' @useDynLib nphawkes
//' @param g a vector of length n(n-1)/2, holding the triggering probabilities
//' @param mu a numeric of length 1, holding the background rate
//' @param n A single integer indicating the number of events in the catalog.
//' @return pbNew a vector of background probabilities
//' @return ptNew a vector of triggering probabilities
//' @export
// [[Rcpp::export]]
List ComputeTriggeringTemporal(NumericVector g, double mu, int n) {
  int n2 = n*(n-1)/2;
  NumericVector pbNew(n);
  NumericVector ptNew(n2);
  
  int ind1 = 0, ind2 = 0;
  for(int i = 0; i < n; i++) {
    double Z = 0;
    for(int j = 0; j < i; j++) {
      Z += g[ind1];
      ind1++;
    }
    for(int j=0; j < i; j++) {
      ptNew[ind2] = (g[ind2]) / (Z+mu);
      ind2++;
    }
    pbNew[i] = mu / (Z+mu);
  }    
  return List::create(Named("pbNew") =  pbNew,
                      Named("ptNew") = ptNew);
}