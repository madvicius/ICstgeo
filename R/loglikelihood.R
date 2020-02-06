loglkl = function(pars,
                  y,s,t,mu=rep(0,length(y)),
                  teigen){
  
  
  
  cov = stcov(
    pars = pars,
    s=s,t=t,
    teigen=teigen
  )
  
  covbias = cov + 10^(-5)*diag(nrow(cov))
  
  out = -1*dmnormC(y,mu,covbias )
  return(out)
}
