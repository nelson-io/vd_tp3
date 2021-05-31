
library(tidyverse)
library(gganimate)
library(lubridate)

data_list <- map(list.files('data',full.names = T), read_csv) %>% 
  set_names(list.files('data') %>% str_extract('.*(?=\\.csv)'))


parsemonth <- function(date){
  month = month(mdy(date))
  if(month %in% 1:9){
    return(paste0('0',as.character(month)))
  }
  else{
    return(as.character(month))
  }
}


pre_plot_data <- data_list$covid_approval_polls_adjusted %>% 
  arrange(mdy(enddate)) %>% 
  transmute(
         year = year(mdy(enddate)),
         month = map_chr(enddate, parsemonth),
         yearmon  = paste0(month(mdy(enddate),label = T,abbr = F),' ',year),
         subject = subject,
         approve = approve_adjusted,
         disapprove = disapprove_adjusted) 


plot_data <- pre_plot_data %>%
  gather(key = 'approve_type', value = 'value', -yearmon, - subject, -year, - month) %>% 
  arrange(year,month)
      
  
  
  
  
  # pivot_longer(cols = c('approve', 'disapprove'))

anim <- ggplot(plot_data,aes(x = subject, y= value, col = approve_type))+
  geom_boxplot(size = 2)+ 
  geom_point(position=position_jitterdodge(),alpha = .1) +
  theme_bw()+
  labs(title = "Aprobación para los candidatos en el período {closest_state}", 
       x = 'Candidato', 
       y = 'Puntaje') +
  transition_states(
    as.integer(paste0(year,month)),
    transition_length = 6,
    state_length = 1)   


animate(anim,renderer = gifski_renderer(),nframes = 800,fps = 20,
        height = 1000, width =500)

anim_save('gifs/boxplots.gif', animation = last_animation())
