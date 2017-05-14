#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Computes the triggering function for a spatial temporal hawkes point process with a homonogenous and stationary background rate.
//' @useDynLib nphawkes
//' @param g A vector of length n(n-1)/2, holding the triggering probabilities for time
//' @param mu A vector of length n(n-1)/2, holding the background probabilities associated with the grid of each point
//' @param kp A single number representing the mean number of events triggered per day
//' @param n A single integer indicating the number of events in the catalog.
//' @export
// [[Rcpp::export]]
List ComputeTriggeringTemporalNS(NumericVector g, double kp, NumericVector mu, int n) {
  int n2 = n*(n-1)/2;
  NumericVector pbNew(n);
  NumericVector ptNew(n2);
  
  int ind1 = 0, ind2 = 0;
  for(int i = 0; i < n; i++) {
    double Z = 0;
    for(int j = 0; j < i; j++) {
      Z += kp * g[ind1];
      ind1++;
    }
    for(int j=0; j < i; j++) {
      ptNew[ind2] = (kp * g[ind2]) / (Z+mu[i]);
      ind2++;
    }
    pbNew[i] = mu[i] / (Z+mu[i]);
  }    
  return List::create(Named("pbNew") =  pbNew,
                      Named("ptNew") = ptNew);
}