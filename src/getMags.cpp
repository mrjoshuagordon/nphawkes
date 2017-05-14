#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Generates marks for all sequences of events in the catalog
//'
//' @useDynLib nphawkes
//' @param m A vector of length n, holding the marks
//' @param n A single integer indicating the number of events  
//' @return mags A vector of length n(n-1)/2 containing the marks associated with each triggering probability
//' @export
// [[Rcpp::export]]
NumericVector getMags(NumericVector m, int n) {
  int n2 = n*(n-1)/2;
  NumericVector mags(n2);
  int ind = 0;
  
  for(int i=0; i < n; i++) {
    for(int j=0; j < i; j++) {
      mags[ind] = m[j];
      ind++;
    }
  }
  return mags;
} 