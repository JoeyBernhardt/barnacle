

### figure out the heights of the ibuttons at each site

library(readxl)
library(tidyverse)
library(janitor)

hi <- read_xlsx("data-raw/ibutton_heights.xlsx") %>% 
	clean_names()


sheep_ibutton_heights <- hi %>% 
	filter(str_detect(pattern = "ibutton", string = replicate)) %>% 
	filter(site == "SheepfarmGI") 


sheep_ibutton_heights %>% 
	group_by(substrate) %>% 
	summarise_each(funs(mean, max, min), height_above_mllw)
