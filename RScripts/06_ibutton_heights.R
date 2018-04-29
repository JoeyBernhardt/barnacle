

### figure out the heights of the ibuttons at each site

library(readxl)
library(tidyverse)
library(janitor)

hi <- read_xlsx("data-raw/ibutton_heights.xlsx") %>% 
	clean_names()


sheep_ibutton_heights <- hi %>% 
	filter(str_detect(pattern = "ibutton", string = replicate)) %>% View
	filter(site == "SheepfarmGI") 


sheep_ibutton_heights %>% 
	group_by(substrate) %>% 
	summarise_each(funs(mean, max, min), height_above_mllw)

ibutton_heights <- hi %>% 
	filter(str_detect(pattern = "ibutton", string = replicate)) %>% 
	mutate(tide_site = case_when(site == "WelburyGI" ~ "Welbury",
															 site == "SookesGI1" ~ "Sooke",
															 site == "SookesGI2" ~ "Sooke",
															 site == "EagleCoveGI" ~ "Eagle",
															 site == "SheepfarmGI" ~ "Sheepfarm",
															 site == "MotelTofino" ~ "Sketchy",
															 site == "CrabInnTofino" ~ "Crab",
															 site == "AquariumUkie1" ~ "Ukie",
															 site == "ToquartBay" ~ "Toquart",
															 site == "28thVan" ~ "28th",
															 site == "CaulfieldCoveVan" ~ "Caulfield",
															 site == "WhytecliffVan" ~ "Whyte",
															 site == "CopperCoveVan" ~ "Copper"))


unique(ibutton_heights$tide_site)

ih_sum <- ibutton_heights %>% 
	group_by(substrate, tide_site) %>% 
	summarise_each(funs(min, mean), height_above_mllw)

write_csv(ih_sum, "data-processed/ibutton_heights_summary.csv")
