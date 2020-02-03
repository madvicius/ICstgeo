#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix distC(const NumericMatrix & x){
  
  unsigned int xnrow = x.nrow(), i = 0, j = 0;
  NumericMatrix dist(xnrow,xnrow);
  double d;
  for(i = 0; i < xnrow; ++i){
    NumericVector x1 = x.row(i);
    for(j = 0; j <= i; ++j){
      
      d = sqrt(sum(pow( x1 - x.row(j) ,2)));
      dist(i,j) = d;
      dist(j,i) = d;
    }
  }
  return dist;
}