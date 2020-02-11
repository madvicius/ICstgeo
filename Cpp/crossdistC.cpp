#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix crossdistC(const NumericMatrix & x,const NumericMatrix & y){
  int xnrow = x.nrow(), ynrow = y.nrow(),i = 0, j = 0;
  NumericMatrix dist(xnrow,ynrow);
  for(i = 0; i < xnrow; i++){
    NumericVector x1 = x.row(i);
    for(j = 0; j < ynrow; j++){
      dist(i,j) = sqrt( sum(pow( x1-y.row(j) ,2)) );
    }
  }
  return dist;
}