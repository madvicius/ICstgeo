loglkl = function(pars,
                  y,s,t,mu=rep(0,length(y)),
                  timecov){
  cov = stcov(
    pars = pars,
    s=s,t=t,
    timecov=timecov
  )
  
  covbias = cov + 10^(-4)*diag(nrow(cov))
  out = -1*dmnormC(y,mu,covbias )
  return(out)
}
