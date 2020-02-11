library(magrittr)
library(tidyverse)
library(fda)



# Data Manipulation -------------------------------------------------------


data = read_csv('Data/estacoes.csv')[,-1] %>%
  arrange(Cidade,Data)

data %>%
  ggplot(aes(Data,Precipitacao)) + 
  geom_line(aes(group=Cidade,col=Cidade)) + 
  theme_bw()+
  theme(legend.position = 'none') +
  scale_x_date(date_breaks = '1 month',date_labels = "%b %Y") + 
  facet_wrap(~Cidade)


aux = data %>% spread(Data,Precipitacao) %>%
  select(-Lat,-Long) 



matdata = aux %>% select(-Cidade) %>% 
  as.matrix() %>% 
  set_rownames(aux$Cidade) %>% t()

id = which(is.na(matdata),arr.ind = T)[,2] %>% unique()

id  
matdata = matdata[,-id]

# Function Analises -------------------------------------------------------


bs = create.bspline.basis(rangeval = c(1,15),
                          norder = 3,nbasis=5)

bsaux = predict(bs,1:15)

bscoef = solve(crossprod(bsaux)+diag(ncol(bsaux))*10^(-20),
               crossprod(bsaux,matdata)) 

fda = fd(bscoef,bs)
predict(fda,1:15)
plot(fda)


pcfa = pca.fd(fda,nharm = 3)


par(mfrow=c(1,2))

matdata %>% 
  matplot(type='l')

predict(
  fd(pcfa$harmonics$coefs,bs),
  seq(1,15,.1)
) %>%
  matplot(type= 'l')

par(mfrow=c(1,1))



# Kriging -----------------------------------------------------------------


library("rnaturalearth")
library("rnaturalearthdata")
library('ggspatial')

krigdata = data %>%
  filter(Cidade %in% colnames(matdata),
         Data == '2018-01-01') %>%
  select(-Data)
world <- ne_countries(scale = "medium",returnclass = "sf")

ggplot(data = world) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") + 
  theme_bw()+
  coord_sf(xlim = c(-80, -30), 
           ylim = c(-35, -10), expand = FALSE) + 
  geom_point(data=krigdata,
             aes(Long,Lat,col=Precipitacao)) + 
  viridis::scale_color_viridis()+
  annotation_scale(location = "bl")





library(Rcpp)
library(RcppEigen)

sourceCpp('Cpp/distC.cpp')
sourceCpp('Cpp/crossdistC.cpp')
sourceCpp('Cpp/covC.cpp')
sourceCpp('Cpp/dmvnorm.cpp')

loglikelihood <- function(pars,x,dist,mu=rep(0,length(x))){
  C = covexp(dist,pars)
  
  return(dmnormC(x,mu,C))
  
}

dist = krigdata %>% 
  select(Lat,Long) %>%
  as.matrix %>%
  distC()
x = krigdata$Precipitacao

res = optim(c(.0001,var(x),1),fn=loglikelihood,
            x=x,dist=dist,
            mu = rep(0,length(x)),
            method = "L-BFGS-B",
            control = list(trace=5,fnscale=-1),
            lower = c(0,0,0)+.000001,  
            upper = c(Inf,Inf,Inf) )


snew = expand.grid(Long =seq(-35,-10,1),
                   Lat = seq(-80, -30 ,1)) %>%
  as.matrix()

Cxx = covexp(dist,res$par)
Cyy = covexp(distC(snew),res$par)
Cxy = covexp(crossdistC(dist,distC(snew)),res$par)


xnew = t(Cxy) %*% solve(Cxx,x)%>% as.numeric()

tibble(snew[,1],snew[,2],xnew) %>%
  set_names(c('Lat','Long','Prec')) %>%
  ggplot(aes(Long,Lat,fill=Prec)) +
  geom_tile() 



























