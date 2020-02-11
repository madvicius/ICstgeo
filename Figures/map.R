library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library('ggspatial')
library(brazilmaps)


data = read_csv('Data/estacoes.csv')[,-1] 

unique(data$Data) %>% sort()

timecity = data %>% spread(Data,Precipitacao) %>%
  select(-Lat,-Long,-Cidade)%>% 
  as.matrix() %>% 
  set_rownames(unique(data$Cidade)) %>% 
  t()
idcol = which(is.na(timecity),arr.ind = TRUE)[,2] %>% unique()
timecity= timecity[,-idcol]

mapdata = data %>%
  filter(Cidade %in% colnames(timecity),
         Data == '2018-01-01') %>%
  select(-Data)

br = get_brmap("State") 

brplot = ggplot(data = br) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") + 
  theme_bw()+
  coord_sf(xlim = c(-55, -38), 
           ylim = c(-25, -12)) + 
  geom_point(data=mapdata,
             aes(Long,Lat),size=5,shape='+') + 
  viridis::scale_color_viridis()+
  annotation_scale(location = "bl")


ggsave('Figures/map.png')






