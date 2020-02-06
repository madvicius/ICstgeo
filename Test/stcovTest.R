library(Rcpp)
library(RcppEigen)


# Datasets e Variaveis ----------------------------------------------------
sourceCpp('Cpp/distC.cpp')
sourceCpp('Cpp/covC.cpp')
sourceCpp('Cpp/logLikelihood.cpp')
source('R/eigenfunctions.R')
st = read.csv('Data/precipitacao-verao-2018.csv',
              stringsAsFactors = FALSE) %>%
  mutate(Periodo = as.Date(Periodo))
y = st$Precipitacao
s = st[c('Latitude','Longitude')] %>% unique() %>% as.matrix()
t = 1:9

# Covariancia -------------------------------------------------------------
nfunc = 2

Y = matrix(y,ncol=length(t),byrow = TRUE) %>% t()

teigen = timeFunction(Y,t,basetype = 'spline',
                      rangeval = c(1,9),nbasis = 6,norder = 3,
                      nfunc = nfunc)


cov = stcov(pars = c( rep(0.1,nfunc), #nugget
                      rep(5,nfunc), #sigma
                      rep(.3,nfunc)), #phi
            s=s,t=t,teigen=teigen
)
dim(cov)

plot(eigen(cov)$values)
