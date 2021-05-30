library(rio)
library(tidyverse)

data_list <- map(list.files('data',full.names = T), import) %>% 
  set_names(list.files('data') %>% str_extract('.*(?=\\.csv)'))




