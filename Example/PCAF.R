library(tidyverse)
library(magrittr)
library(fda)

data = read_csv('Data/estacoes.csv')[,-1] 

unique(data$Data) %>% sort()

timecity = data %>% spread(Data,Precipitacao) %>%
  select(-Lat,-Long,-Cidade)%>% 
  as.matrix() %>% 
  set_rownames(unique(data$Cidade)) %>% 
  t()

idcol = which(is.na(timecity),arr.ind = TRUE)[,2] %>% unique()

timecity= timecity[,-idcol]

matplot(timecity,type='l')


bs = create.fourier.basis(c(1,15),
                          nbasis = 5)

bscoef = solve(
  crossprod(predict(bs,1:15)),
  crossprod(predict(bs,1:15),timecity)
)





fda = fd(coef = bscoef,basisobj = bs)
#Lfd(2,fd2list(fda))


pcaf = pca.fd(fda,nharm = 3)
pcafda = fd(pcaf$harmonics$coefs ,
            bs)
plot(pcafda)


par(mfrow=c(1,2))
matplot(timecity,type='l')
plot(fda)

plot(fda)
plot(pcafda)
par(mfrow=c(1,1))


preciptime = data %>% 
  filter(Cidade %in% colnames(timecity)) %>%
  ggplot(aes(Data,Precipitacao)) + 
  geom_line(aes(group=Cidade,col=Cidade)) + 
  theme_classic()+
  theme(legend.position = 'none') + 
  scale_y_continuous(limits = c(-60,500))+
  scale_x_date(date_breaks = '1 month',date_labels = '%b%y');preciptime

ggsave('Figures/preciptime.png',plot = preciptime)  




time = data$Data %>% unique() %>% sort()

res=100
timegrid = tibble(Data = seq(time[1],time[15],length.out = res),
                  id = seq(1,15,length.out = res)) 


fourierfit = predict(fda,timegrid$id) %>%
  matrix(ncol = 38) %>% 
  as_tibble() %>% 
  set_names(colnames(timecity))%>%
  bind_cols(timegrid) %>%
  gather(key = 'Cidade',value = 'Precipitacao',-Data,-id) %>%
  select(-id) %>%
  ggplot(aes(Data,Precipitacao)) + 
  geom_line(aes(group=Cidade,col=Cidade)) + 
  theme_classic()+
  theme(legend.position = 'none') + 
  scale_x_date(date_breaks = '1 month',date_labels = '%b%y')+ 
  scale_y_continuous(limits = c(-60,500)); fourierfit

pcafit = predict(pcafda,timegrid$id) %>%
  matrix(ncol = 3) %>% 
  as_tibble() %>% 
  set_names(paste0('F',1:3))%>%
  bind_cols(timegrid) %>%
  gather(key = 'Funcao',value = 'Precipitacao',-Data,-id) %>%
  select(-id) %>%
  ggplot(aes(Data,Precipitacao)) + 
  geom_line(aes(group=Funcao,col=Funcao)) +
  theme_classic()+ 
  theme(legend.position = c(.1,0.2)) + 
  guides(colour = guide_legend(title=''))+
  scale_colour_discrete(labels=expression(hat(varphi)[1],
                                          hat(varphi)[2], 
                                          hat(varphi)[3]))+
  scale_x_date(date_breaks = '1 month',date_labels = '%b%y');pcafit



ggsave('Figures/fourierfit.png',plot = fourierfit)
ggsave('Figures/pcafit.png',plot = pcafit)












