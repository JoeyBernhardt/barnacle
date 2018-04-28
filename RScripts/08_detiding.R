
library(tidyverse)
library(janitor)

## de-tiding the ibutton data

all_i <- read_csv("data-processed/all_ibutton_data.csv")
all_tides <- read_csv("data-processed/all_sites_1min_tides.csv")
tide_sites <- read_xls("data-raw/tide_data_sites.xls") %>% 
	clean_names()

tide_sites2 <- tide_sites %>% 
	select(site, tide_data_site)

tide_sites3 <- tide_sites2 %>% 
	mutate(tide_site = case_when(tide_data_site == "Stopper Islands" ~ "stopper",
																		tide_data_site == "Tofino" ~ "tofino",
															 tide_data_site == "Ucluelet" ~ "ucluelet",
															 tide_data_site == "Fulford Harbour" ~ "fulford",
															 tide_data_site == "Ganges Harbour" ~ "ganges",
															 tide_data_site == "Point Atkinson" ~ "atkinson",
															 tide_data_site == "Bamfield" ~ "bamfield"))

unique(all_i$Site)

all_i2 <- all_i %>% 
	mutate(tide_site = case_when(Site == "Crab" ~ "tofino",
															 Site == "Sketchy" ~ "tofino",
															 Site == "Toquart" ~ "stopper",
															 Site == "Ukie" ~ "ucluelet",
															 Site == "Eagle" ~ "fulford",
															 Site == "Sheepfarm" ~ "fulford",
															 Site == "Sooke" ~ "fulford",
															 Site == "Welbury" ~ "ganges",
															 Site == "28th" ~ "atkinson",
															 Site == "Caulfied" ~ "atkinson",
															 Site == "Copper" ~ "atkinson",
															 Site == "Whyte" ~ "atkinson"))


atkinson_ibutton <- all_i2 %>% 
	filter(tide_site == "atkinson") %>% 
	rename(site_id = site)

atkinson_tides <- all_tides %>% 
	filter(site == "atkinson")

all_at <- left_join(atkinson_ibutton, atkinson_tides, by = "date") %>% 
	filter(!is.na(height))

all_at %>% 
	mutate(emersed = case_when)
