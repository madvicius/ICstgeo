library(tidyverse)
library(magrittr)
library("rnaturalearth")
library("rnaturalearthdata")
library('ggspatial')
library(brazilmaps)
library(geoR)
library(Rcpp)
library(RcppEigen)

sourceCpp('Cpp/distC.cpp')
sourceCpp('Cpp/crossdistC.cpp')
sourceCpp('Cpp/covC.cpp')
sourceCpp('Cpp/dmvnorm.cpp')
source('R/loglikelihood.R')

data = read_csv('Data/estacoes.csv')[,-1] 
timecity = data %>% spread(Data,Precipitacao) %>%
  select(-Lat,-Long,-Cidade)%>% 
  as.matrix() %>% 
  set_rownames(unique(data$Cidade)) %>% 
  t()

idcol = which(is.na(timecity),arr.ind = TRUE)[,2] %>% unique()

timecity= timecity[,-idcol]

spacedata = data %>%
  filter(Cidade %in% colnames(timecity),
         Data == '2018-01-01') %>%
  select(-Data)


dist = spacedata[c('Lat','Long')] %>% as.matrix() %>% distC()
x = spacedata['Precipitacao'] %>% as.matrix



#62097.10114    40.78876
loglikelihood(c(0.001,6000,40),dist=dist,x = x,mu = rep(0,length(x)))

parsout = optim(c(.001,6000,100),fn=loglikelihood,
      x=x,dist=dist,
      mu = rep(0,length(x)),
      method = "L-BFGS-B",
      control = list(trace=5,fnscale=-1),
      lower = c(0,0,.1)+.000001,  #problema do phi
      upper = c(0.001,Inf,Inf) ) #valor do nugget

parsout$par

res=100
snew = expand.grid(Lat = seq(-12, -25,
                           length.out = res),
                   Long = seq(-55, -38,
                           length.out = res))

s = spacedata[c('Lat','Long')] %>% as.matrix()


Cxx = covexp(dist,parsout$par)
Cxy = covexp(crossdistC( as.matrix(snew),s ),parsout$par)


xnew = Cxy %*% solve(Cxx,x)%>% as.numeric()


newdata = snew %>% bind_cols(Precipitacao = xnew) %>% as_tibble()
br = get_brmap("State") 


brplot = ggplot(data = br) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") + 
  theme_bw()+
  coord_sf(xlim = c(-55, -38), 
           ylim = c(-25, -12)) + 
  geom_point(data=spacedata,
             aes(Long,Lat,col=Precipitacao)) + 
  viridis::scale_color_viridis()

krigplot = ggplot(data=br) +
  geom_raster(data=newdata, 
               aes(Long,Lat,fill=Precipitacao)) +
  xlab("Longitude") + ylab("Latitude")+ 
  theme_bw()+
  geom_sf(fill=NA,colour='white') +
  viridis::scale_fill_viridis() +
  coord_sf(xlim = c(-55, -38), 
           ylim = c(-25, -12),expand = FALSE)

gridExtra::grid.arrange(brplot,krigplot,ncol=2)





ggsave('Figures/krigpoint.png',plot = brplot)
ggsave('Figures/krigmap.png',plot = krigplot)













