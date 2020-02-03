library(Rcpp)
library(RcppEigen)


# Datasets e Variaveis ----------------------------------------------------
sourceCpp('Cpp/distC.cpp')
sourceCpp('Cpp/covC.cpp')
sourceCpp('Cpp/logLikelihood.cpp')

st = read.csv('Data/precipitacao-verao-2018.csv',
              stringsAsFactors = FALSE) %>%
  mutate(Periodo = as.Date(Periodo))
y = st$Precipitacao
s = st[c('Latitude','Longitude')] %>% unique() %>% as.matrix()
t = 1:9

# Covariancia -------------------------------------------------------------
nfunc = 2
Y = matrix(y,ncol=length(t),byrow = TRUE) %>% t()

teigen = timeFunction(Y,t,s,
                      rangeval = c(1,9),nbasis = 6,norder = 3,
                      nfunc = nfunc)
timecov = tcrossprod(teigen$functions)

d = distC(s)
nugget = rep(.1,nfunc)
sigma = rep(5,nfunc)
phi = rep(.1,nfunc)

pars = c(nugget,sigma,phi)

for(i in 1:nfunc){
  param = pars[seq(1,3*nfunc,nfunc)+(i-1)]
  spacecov = covexp(dist,param)
  cov = teigen$values[i] * kronecker(timecov,spacecov)
}

# Log Verossimilhança -----------------------------------------------------

dmnormC(y,rep(0,length(y)),cov)
mvtnorm::dmvnorm(y,sigma = cov,log = T)




system.time(
  dmnormC(y,rep(0,length(y)),cov)
)
system.time(
  mvtnorm::dmvnorm(y,sigma = cov,log = T)
)







