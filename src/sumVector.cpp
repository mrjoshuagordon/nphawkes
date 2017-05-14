#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Utility function to sum a vector
//'
//' @useDynLib nphawkes
//' @param v A vector
//' @return sum the sum
//' @export
// [[Rcpp::export]]
double sumVector(NumericVector v) {
  double sum = 0;
  for(int i=0; i < v.size(); i++) {
    sum += v[i];
  }
  return(sum);
}