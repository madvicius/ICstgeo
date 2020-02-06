stcov = function(pars,
                 teigen,
                 s,t
){
  
  nparam = teigen$nfunc
  
  nugget = pars[1:nparam]
  sigma = pars [1:nparam + nparam]
  phi = pars[1:nparam  + 2*nparam]
  
  timecov = tcrossprod(predict(teigen$fd,t))
  d = distC(s)
  
  for(i in 1:nfunc){
    param = c(nugget[i],sigma[i],phi[i])
    spacecov = covexp(d,param)
    cov = teigen$values[i]^2 * kronecker(timecov,spacecov)
  }
  
  return(cov)
  
  
}
