#include <RcppEigen.h>
using namespace Eigen;

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
double dmnormC(VectorXd &x,  VectorXd &mu, MatrixXd &sigma ){
  
  /*
  const int p = x.rows();
  double b; 
  */
  
  double a,c;
  
  x -= mu;
  sigma = sigma.llt().matrixL();
  mu = sigma.triangularView<Lower>().solve(x.transpose());
  
  a = (sigma.diagonal().array().log().sum()); //funciona 
  //b = p*log(2*M_PI); 
  c = (mu.transpose() * mu);  //funciona
  return -1*a /*- 0.5*b*/ - 0.5*c ;
}







