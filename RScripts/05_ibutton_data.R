### read in ibutton data
library(tidyverse)
library(stringr)
library(lubridate)
library(purrr)


i1 <- read_lines("data-raw/2012_Regional_ibutton_data/Outer_coast/Crab bench 2BA532.rtf", skip = 7, n_max = 1720)
i2 <- as.data.frame(i1) %>% 
	separate(i1, into = c("date", "C", "temperature"), sep = ",") %>% 
	mutate(temperature = str_replace(temperature, "\\\\", "")) %>% 
	mutate(date = mdy_hms(date)) %>% 
	mutate(temperature = as.numeric(temperature))

i2 %>% 
	ggplot(aes(x = date, y = temperature)) + geom_point() + geom_line()


### now read in all the ibutton files together

i_files <- c(list.files("data-raw/2012_Regional_ibutton_data/Outer_coast", full.names = TRUE),
						 list.files("data-raw/2012_Regional_ibutton_data/Saltspring", full.names = TRUE),
						 list.files("data-raw/2012_Regional_ibutton_data/Vancouver", full.names = TRUE))
						 

names(i_files) <- i_files %>% 
	gsub(pattern = ".rtf$", replacement = "")

?read_lines()

all_ibuttons <- map(i_files, read_lines, skip = 7, n_max = 1770)

str(all_ibuttons)

allb <- as.data.frame(all_ibuttons) %>% 
	gather(value = "record", key = "site")

allb2 <- allb %>% 
	separate(record, into = c("date", "C", "temperature"), sep = ",") %>% 
	mutate(temperature = str_replace(temperature, "\\\\", "")) %>% 
	mutate(date = mdy_hms(date)) %>% 
	mutate(temperature = as.numeric(temperature))

allb3 <- allb2 %>% 
	mutate(site = str_replace(site, "data.raw.2012_Regional_ibutton_data.", "")) %>%
	mutate(site = str_replace(site, "_", "")) %>% 
	separate(site, into = c("region", "Site", "substrate", "ibutton_id"), remove = FALSE)

unique(allb3$substrate)

allb3 %>% 
	filter(substrate == "2") %>% View


all4 <- allb3 %>% 
	mutate(substrate = ifelse(substrate == "2", "bench", substrate)) %>%
	mutate(substrate = ifelse(substrate == "1", "bench", substrate)) %>% 
	mutate(substrate = ifelse(substrate == "B", "bench", substrate)) %>% 
	mutate(substrate = ifelse(substrate == "Bench", "bench", substrate)) %>% 
	mutate(substrate = ifelse(substrate == "Cobble", "cobble", substrate)) %>% 
	select(-C) %>% 
	mutate(Site = ifelse(Site == "Ukee", "Ukie", Site)) %>% 
	mutate(Site = ifelse(Site == "UkieB", "Ukie", Site)) %>% 
	mutate(Site = ifelse(Site == "eaglebench", "Eagle", Site)) %>% 
	filter(date < "2012-08-29")
	

all4 %>% 
	filter(ibutton_id == "2B9349") %>% 
	filter(date > "2012-07-01", date < "2012-08-01") %>% 
	ggplot(aes(x = date, y = temperature, color = region)) + geom_point() +
	geom_line() +
	facet_wrap( ~ ibutton_id)

unique(all4$Site)
unique(all4$ibutton_id)
max(all4$date)

all4 %>% 
	filter(ibutton_id == "csv") %>% View

write_csv(all4, "data-processed/all_ibutton_data.csv")
	