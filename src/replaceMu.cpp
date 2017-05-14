#include <Rcpp.h> 
#include <R.h>
#include <math.h>
#include <Rinternals.h>
using namespace Rcpp;

//' Utility function to replace elements in vector mu with indices specified in vector of vectors D 
//'
//' @useDynLib nphawkes
//' @param mu A vector of length nbins_x  * nbins_y holding the background rate in each cell
//' @param D A vector of length nbins_x  * nbins_y holding vectors with event indices if they fall within the grid cell 
//' @param n A single integer indicating the number of events in the catalog
//' @return muvec A vector length n, holding the updated probabilities 
//' @export
// [[Rcpp::export]]
NumericVector replaceMu(NumericVector mu, std::vector<std::vector<int> > D, int n){
  NumericVector muvec(n);
    for(int i = 0; i < D.size(); i++){
      if(D[i][0] != -1) {
        for(int j = 0; j < D[i].size(); j++) {
          muvec[D[i][j]] = mu[i];
        }
      }
    }
  return muvec;
}