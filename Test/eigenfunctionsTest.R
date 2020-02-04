library(magrittr)
library(tidyverse)
library(fda)

st = read.csv('Data/precipitacao-verao-2018.csv',
              stringsAsFactors = FALSE) %>%
  mutate(Periodo = as.Date(Periodo))
source('R/eigenfunctions.R')

y = st$Precipitacao
Y = matrix(y,ncol=9,byrow = TRUE) %>% t()
#t = st$Periodo
s = st[c('Latitude','Longitude')] %>% unique()
t = 1:9

teigen = timeFunction(Y,t,s,
             rangeval = c(1,9),nbasis = 6,norder = 3)






