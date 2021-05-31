library(gridExtra)
library(viridis)
library(plotly)

p1 <- dataltv %>% 
  filter(!is.na(model)) %>% 
  gather(key = 'tipo', value = 'volumen_prepago', -c(ltv_bin, UPB)) %>% 
  mutate(volumen_prepago = as.numeric(str_remove_all(volumen_prepago,'%'))/100) %>% 
  ggplot()+
  geom_col(aes(x = as.factor(ltv_bin), y = volumen_prepago, fill = tipo), position = 'dodge')+
  theme_bw()+
  ggtitle("Performance del modelo vs. datos actuales por bin")+
  scale_fill_manual(values = c('gray', 'steelblue'))+
  scale_y_continuous(labels = scales::percent)+
  xlab('Bin de LTV')+
  ylab('Volumen de Prepagos en tiempo real')+ 
  theme(legend.position="top")


p2 <- dataltv %>% 
  filter(!is.na(model)) %>% 
  gather(key = 'tipo', value = 'volumen_prepago', -c(ltv_bin, UPB)) %>% 
  mutate(UPB = as.numeric(str_remove_all(str_remove_all(UPB, '\\$'), '\\,'))/1000,
         volumen_prepago = as.numeric(str_remove_all(volumen_prepago,'%'))/100) %>% 
  ggplot()+
  geom_col(aes(x = as.factor(ltv_bin), y = UPB), fill = '#389003')+
  theme_bw()+
  xlab('Bin de LTV')+
  ylab('UPB (en miles de pesos)')

grid.arrange(p1,p2,nrow = 2)


