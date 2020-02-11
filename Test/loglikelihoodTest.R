library(Rcpp)
library(RcppEigen)
library(magrittr)
library(tidyverse)

# Datasets e Variaveis ----------------------------------------------------
sourceCpp('Cpp/distC.cpp')
sourceCpp('Cpp/covC.cpp')
sourceCpp('Cpp/logLikelihood.cpp')
source('R/loglikelihood.R')
source('R/eigenfunctions.R')
source('R/stcov.R')


st = read.csv('Data/precipitacao-verao-2018.csv',
              stringsAsFactors = FALSE) %>%
  mutate(Periodo = as.Date(Periodo))
y = st$Precipitacao
s = st[c('Latitude','Longitude')] %>% unique() %>% as.matrix()
t = 1:9

# Covariancia -------------------------------------------------------------
nfunc = 1
Y = matrix(y,ncol=length(t),byrow = TRUE) %>% t()

timecov = timeFunction(Y,t,basetype = 'spline',
                      rangeval = c(1,9),nbasis = 6,norder = 3,
                      nfunc = nfunc)

pars = c( rep(0.25,nfunc), #nugget
          rep(var(y),nfunc), #sigma
          rep(1,nfunc)) #phi


loglkl(pars = pars,y = y,t = t,s = s,timecov = timecov)

optim(pars,loglkl,method = 'L-BFGS-B',
      lower = rep(.01,length(pars)),
      y = y,t = t,s = s,timecov = timecov
      )

























