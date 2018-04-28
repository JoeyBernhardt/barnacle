

### tide sites
library(janitor)
library(tidyverse)
library(lubridate)

tide_sites <- read_xls("data-raw/tide_data_sites.xls") %>% 
	clean_names()


unique(tide_sites$tide_data_site)


fulford <- read_xlsx("data-raw/fulford_1min_tide.xlsx", col_names = "tide") %>% 
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height)) %>% 
	mutate(site = "fulford")


stopper <- read_tsv("data-raw/stopper_islands_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height)) %>% 
	mutate(site = "stopper")

ucluelet <- read_tsv("data-raw/ucluelet_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height)) %>% 
	mutate(site = "ucluelet")

tofino <- read_xlsx("data-raw/tofino_1min_tide.xlsx", col_names = "tide") %>% 
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height)) %>% 
	mutate(site = "tofino")

bamfield <- read_tsv("data-raw/bamfield_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height)) %>% 
	mutate(site = "bamfield")

ganges <- read_tsv("data-raw/ganges_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height)) %>% 
	mutate(site = "ganges")

atkinson <- read_tsv("data-raw/atkinson_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height)) %>% 
	mutate(site = "atkinson")

all_tides <- bind_rows(atkinson, ganges, fulford, bamfield, tofino, ucluelet, stopper)
write_csv(all_tides, "data-processed/all_sites_1min_tides.csv")

