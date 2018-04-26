### read in ibutton data
library(tidyverse)
library(stringr)
library(lubridate)


i1 <- read_lines("data-raw/2012_Regional_ibutton_data/Outer_coast/Crab bench 2BA532.rtf", skip = 7)
i2 <- as.data.frame(i1) %>% 
	separate(i1, into = c("date", "C", "temperature"), sep = ",") %>% 
	mutate(temperature = str_replace(temperature, "\\\\", "")) %>% 
	mutate(date = mdy_hms(date)) %>% 
	mutate(temperature = as.numeric(temperature))

i2 %>% 
	ggplot(aes(x = date, y = temperature)) + geom_point() + geom_line()
