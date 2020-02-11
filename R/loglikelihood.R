
loglikelihood <- function(pars,x,dist,mu=rep(0,length(x))){
  C = covexp(dist,pars)
  
  return(dmnormC(x,mu,C))
  
}


