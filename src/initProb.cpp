#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Initialize triggering and background probabilities 
//'
//' @useDynLib nphawkes
//' @param n A single integer indicating the number of events in the catalog.
//' @export
// [[Rcpp::export]]
List initProb(int n) {
  int n2 = n*(n-1)/2;
  NumericVector pbNew(n);
  NumericVector ptNew(n2);
  int ind1 = 0, ind2 = 0;
  for(int i = 1; i <= n ; i++) {
      for(int j = 1; j < i; j++) {
        ptNew[ind1] = 1.0/(double)i;
        ind1++;
      }
      pbNew[ind2++] = 1.0/(double)i;
    }
  return List::create(Named("pbNew") =  pbNew,
                      Named("ptNew") = ptNew);
}