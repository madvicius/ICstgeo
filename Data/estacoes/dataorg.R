library(magrittr)
library(tidyverse)
files = dir('Data/estacoes')
files = files[-which(str_detect(files,'.R'))]
length(files)

for( i in seq_along(files) ){
  
  str = read_file(paste0('Data/estacoes/',files[i]),
                  locale = locale(encoding = 'latin1')) %>%
    str_remove_all('\r')%>% str_split(pattern = '\n',simplify = TRUE) %>%
    as.character()
  aux = str[5:6] %>%  
    str_extract( "\\-*\\d+\\.*\\d*") %>% 
    as.numeric()%>%
    matrix(ncol=2)
  
  if(i == 1){
    data=aux
  }
  else{
    data=rbind(data,aux)
  }
  
}

data %<>% as_tibble() %>% set_colnames(c('Lat','Long'))


data %<>% mutate(Cidade = str_remove_all(files,'.txt'))



for(i in seq_along(files)){
  
  aux2 = read_csv2(paste0('Data/estacoes/',files[i]),
                   skip = 16)[,-8] %>%
    mutate(Cidade =  str_remove_all(files[i],'.txt'))%>%
    filter(Hora == '1200') %>%
    select(Cidade,Data,Precipitacao)
  
  if(i ==1){
    metdata = aux2
  }
  else{
    metdata = rbind(metdata,aux2)
  }
}



stdata = data %>% inner_join(metdata) %>%
  mutate(Precipitacao = as.numeric(Precipitacao))

stdata

stdata %<>% 
  separate(Data,c('Dia','Mes','Ano')) %>%
  group_by(Lat,Long,Cidade,Mes,Ano) %>%
  summarise(Precipitacao = sum(Precipitacao,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Dia = '01') %>%
  unite('Data',Dia,Mes,Ano,sep='/') %>%
  mutate(Data=as.Date(Data,'%d/%m/%Y'))

stdata %>% write.csv('Data/estacoes.csv')





