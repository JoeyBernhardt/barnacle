

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
	mutate(height = as.numeric(height))


stopper <- read_tsv("data-raw/stopper_islands_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height))

ucluelet <- read_tsv("data-raw/ucluelet_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height))

tofino <- read_xlsx("data-raw/tofino_1min_tide.xlsx", col_names = "tide") %>% 
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height))

bamfield <- read_tsv("data-raw/bamfield_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height))

ganges <- read_tsv("data-raw/ganges_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height))

atkinson <- read_tsv("data-raw/atkinson_1min_tide.tsv", col_names = "tide") %>%  
	separate(tide, into = c("date", "height"), sep = " PDT ") %>% 
	mutate(date = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height))
