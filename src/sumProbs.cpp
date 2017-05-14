#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Utility function to check that the rows of the matrix for the background and triggering probabilities sum to 1
//'
//' @useDynLib nphawkes
//' @param vt A vector of length n(n-1)2, holding the triggering probabilities
//' @param vb A vector of length n, holding the background probabilities
//' @param sz A single integer indicating the number of events  
//' @export
// [[Rcpp::export]]
bool sumProbs(NumericVector vt, NumericVector vb, int sz) {
  int ind = 0;
  for(int i=0; i < sz; i++) {
    double sum = 0;
    for(int j=0; j < i; j++) {
      sum += vt[ind];
      ind++;
    }
    sum += vb[i];
    if((sum < 1 - 1e-04) || (sum > 1.0 + 1e-04)) {
      return(false);
    }
  }
  return(true);
}