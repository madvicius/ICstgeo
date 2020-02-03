#include <RcppEigen.h>
using namespace Eigen;

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
MatrixXd covexp(MatrixXd &dist, const VectorXd &pars ){
  
  double nugget = pars(0), sigma = pars(1), phi = pars(2);
  const int r=dist.rows(), c=dist.cols() ;
  
  dist /=  -phi;
  dist = sigma * dist.array().exp().matrix();
  
  if( r==c){
    dist.diagonal().array() += nugget;
  }
  
  return dist;
}