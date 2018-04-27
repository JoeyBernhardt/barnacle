library(cowplot)
library(broom)
library(tidyverse)
library(viridis)


emersion <- read_csv("data-processed/emersion_time_upperlimits.csv")
emersion_noBoulder <- emersion %>% 
	filter(substrate != "boulder")

emersion_noBoulder <- emersion_noBoulder %>% 
	mutate(emersion_time_hours = ((total/60)/29)) ## don't know why we divide by 29 here...look back into this


emersion_noBoulder$Region <- ordered(emersion_noBoulder$Region, levels = c("gulf_islands", "vancouver", "outer_coast"))


emersion_noBoulder %>% 
	ggplot(data = ., aes(x = Region, y = mean_height, fill = factor(substrate))) + geom_boxplot() + 
	ylab("height above MLLW, m") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold"))


emersion_noBoulder %>% 
	ggplot(data = ., aes(x = Region, y = emersion_time_hours, fill = factor(substrate))) + geom_boxplot() + 
	ylab("Daily emersion time (hours)") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/emersion_time_boxplot.pdf")

emersion_noBoulder %>% 
	ggplot(aes(x = substrate, y = emersion_time_hours, fill = factor(substrate))) + geom_bar(stat = "identity") + 
	ylab("hours of emersion time per day") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) + facet_wrap( ~ Site) +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/emersion_time_barcharts.pdf", width = 12, height = 8)


emersion_noBoulder %>% 
	ggplot(aes(x = substrate, y = emersion_time_hours, color = factor(substrate))) + geom_point() + 
	ylab("hours of emersion time per day") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) +
	# facet_wrap( ~ Site) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)

mod1 <- lm(emersion_time_hours ~ substrate + Site, data = emersion_noBoulder)
summary(mod1)

levels(emersion_noBoulder$Region)
unique(emersion_noBoulder$Site)



em <- emersion_noBoulder %>% 
	mutate(isite = NA) %>% 
	mutate(isite = ifelse(Site == "CrabInnTofino", "Crab", isite)) %>% 
	mutate(isite = ifelse(Site == "MotelTofino", "Sketchy", isite)) %>% 
	mutate(isite = ifelse(str_detect(Site, "Toquart"), "Toquart", isite)) %>%
	mutate(isite = ifelse(grepl("Eagle", Site), "Eagle", isite)) %>% 
	mutate(isite = ifelse(grepl("Sheepfarm", Site), "Sheepfarm", isite)) %>% 
	mutate(isite = ifelse(grepl("Uk", Site), "Ukie", isite)) %>% 
	mutate(isite = ifelse(grepl("Whyte", Site), "Whyte", isite)) %>%
	mutate(isite = ifelse(grepl("Sooke", Site), "Sooke", isite)) %>% 
	mutate(isite = ifelse(grepl("28th", Site), "28th", isite)) %>% 
	mutate(isite = ifelse(grepl("Caulfield", Site), "Caulfield", isite)) %>% 
	mutate(isite = ifelse(grepl("Welbury", Site), "Welbury", isite)) %>% 
	mutate(isite = ifelse(grepl("Copper", Site), "Copper", isite)) %>% 
	unite(col = "site_substrate", remove = FALSE, isite, substrate)



	
all4 <- read_csv("data-processed/all_ibutton_data.csv")



all5 <- all4 %>% 
	mutate(day = date(date)) %>% 
	group_by(substrate, Site, region, ibutton_id, day) %>% 
	summarise_each(funs(max, mean), temperature) %>% 
	group_by(substrate, Site, region, ibutton_id) %>% 
	summarise_each(funs(mean, max), temperature_max) %>% 
	unite(col = "site_substrate", remove = FALSE, Site, substrate)

all5 %>% 
	ggplot(aes(x = Site, y = temperature_max_mean, color = substrate)) + geom_point()

em2 <- left_join(em, all5, by = "site_substrate")	

em2 %>% 
	# filter(isite == "Sheepfarm") %>% 
	ggplot(aes(x = temperature_max_mean, y = emersion_time_hours, color = substrate.x)) + geom_point() +
	facet_wrap(~ substrate.x) + geom_smooth(method = "lm") +
	xlab("Mean daily max temperature (°C)") + ylab("Daily emersion time (hours)") +
	scale_color_viridis(discrete = TRUE, begin = 0.5, end = 0.9) 
ggsave("figures/emersion_time_temp.pdf")




all5 %>% 
	ggplot(aes(x = Site, y = temperature_max_mean, color = substrate, fill = substrate)) + geom_boxplot() +
	scale_color_viridis(discrete = TRUE, begin = 0.5, end = 0.9) +
	scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 0.9)

### something weird is going on here...
## Eagle, sheepfarm, Sooke and Welbury all have higher average daily max temps on bench then on cobble, weird!!


all6 <- all4 %>% 
	filter(date > "2012-06-30", date < "2012-08-20") %>%
	filter(!grepl("copy", site)) %>% 
	mutate(day = date(date)) %>% 
	mutate(hour = hour(date)) %>% 
	unite(col = "day_hour", day, hour, remove = FALSE) %>%
	mutate(hour_day = ymd_h(day_hour)) %>% 
	group_by(region, hour_day, Site, substrate, ibutton_id) %>% 
	summarise_each(funs(mean), temperature) %>%
	mutate(above_25 = ifelse(temperature > 25, 1, 0)) %>% 
	mutate(above_20 = ifelse(temperature > 20, 1, 0)) %>% 
	mutate(above_30 = ifelse(temperature > 30, 1, 0)) %>% 
	mutate(above_35 = ifelse(temperature > 35, 1, 0)) %>% 
	mutate(above_40 = ifelse(temperature > 40, 1, 0)) %>% 
	mutate(above_45 = ifelse(temperature > 45, 1, 0))

all7 <- all6 %>% 
	group_by(region, Site, substrate, ibutton_id) %>% 
	summarise_each(funs(sum), above_25, above_20, above_30, above_35, above_40, above_45) %>% 
	ungroup()

all7 %>% 
	ggplot(aes(x = Site, y = above_20, color = substrate)) + geom_boxplot() +
	scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.9)

str(all6)	

all4 %>% 
	ungroup() %>% 
	mutate(day = date(date)) %>% 
	mutate(hour = hour(date)) %>% 
	unite(col = "day_hour", day, hour, remove = FALSE) %>%
	mutate(hour_day = ymd_h(day_hour)) %>%
	filter(Site == "Sooke") %>% 
	# filter(day > "2012-08-05", date < "2012-08-15") %>% 
	group_by(substrate, hour_day) %>% 
	summarise_each(funs(mean, max), temperature) %>% 
	ggplot(aes(x = hour_day, y = temperature_max, color = substrate)) + geom_point() +
	geom_line() +
	geom_hline(yintercept = 45) +
	scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.9) 
ggsave("figures/sheepfarm_temps.pdf", width = 20, height = 4)
	
fulford <- read_csv("data-raw/fulford_tide.csv") 
ganges <- read_csv("data-raw/ganges_tide.csv") 

names(fulford) <- "tide"
names(ganges) <- "tide"

ful2 <- fulford %>% 
	separate(tide, into = c("date", "height"), sep = " PDT ") %>%
	mutate(date = as.character(date)) %>% 
	mutate(day = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height))

ful3 <- ful2 %>% 
	filter(day > "2012-08-05", day < "2012-08-15") %>% 
	mutate(height2 = height*2.3) %>% 
	filter(date > "2012-08-05", date < "2012-08-06") 

gan2 <- ganges %>% 
	separate(tide, into = c("date", "height"), sep = " PDT ") %>%
	mutate(date = as.character(date)) %>% 
	mutate(day = ymd_hm(date)) %>% 
	mutate(height = as.numeric(height))

gan3 <- gan2 %>% 
	filter(day > "2012-08-05", day < "2012-08-15") %>% 
	mutate(height2 = height*2.3)

unique(all6$Site)

all8 <- all6 %>% 
	mutate(day = date(date)) %>% 
	mutate(hour = hour(date)) %>% 
	unite(col = "day_hour", day, hour, remove = FALSE) %>%
	mutate(hour_day = ymd_h(day_hour)) %>%
	filter(Site == "Sheepfarm") %>% 
	filter(date > "2012-08-05", date < "2012-08-15") %>%
	group_by(substrate, hour_day) %>% 
	summarise_each(funs(mean, max), temperature) %>% 
	ungroup()

sheep <- all4 %>% 
	mutate(day = date(date)) %>% 
	mutate(hour = hour(date)) %>% 
	unite(col = "day_hour", day, hour, remove = FALSE) %>%
	mutate(hour_day = ymd_h(day_hour)) %>%
	filter(Site == "Sooke") %>% 
	filter(date > "2012-08-05", date < "2012-08-29") 
ful3 <- ful2 %>% 
	mutate(height2 = height*2.3) %>% 
	filter(date > "2012-08-05", date < "2012-08-29") 
	

ggplot() + geom_point(aes(color = substrate, x = hour_day, y = temperature), data = sheep) +
	geom_line(aes(color = substrate, x = hour_day, y = temperature, group = ibutton_id), data = sheep) +
	geom_line(data = ful3, aes(x = day, y = height2), size = 0.5) +
	scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.9) +
	geom_hline(yintercept = 10) +
	geom_hline(yintercept = 17)
ggsave("figures/sheepfarm_temps.pdf", width = 20, height = 4)
ggsave("figures/welbury_temps.pdf", width = 20, height = 4)
ggsave("figures/sookes_temps.pdf", width = 20, height = 4)

ful4 <- ful2 %>% 
	filter(day > "2012-08-05", day < "2012-08-07")

str(ful4)

ful3 %>% 
	ggplot(aes(x = day, y = height)) + geom_point()


all4sub <- all4 %>% 
	filter(date > "2012-08-05", date < "2012-08-07")

### plot the temp data from Copper Cove
ggplot() + geom_point(aes(color = substrate, x = date, y = temperature), data = filter(all4sub, Site == "Copper")) +
	geom_line(aes(color = substrate, x = date, y = temperature, group = ibutton_id), data = filter(all4sub, Site == "Copper")) +
	# geom_line(data = ful3, aes(x = day, y = height2), size = 0.5) +
	scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.9)


### ok let's try to get the monthly maxes


all_months <- all4 %>% 
	mutate(month = month(date)) %>% 
	group_by(region, Site, substrate, ibutton_id, month) %>% 
	summarise_each(funs(max), temperature) %>% 
	group_by(region, Site, substrate, ibutton_id) %>% 
	summarise_each(funs(max), temperature) 

all_months %>% 
	ggplot(aes(x = Site, y = temperature, color = substrate, fill = substrate)) + geom_boxplot() +
	scale_color_viridis(discrete = TRUE, begin = 0.5, end = 0.9) +
	scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 0.9)
	

### now let's try to see if the cobbles heat up faster

all_change <- all4 %>% 
	group_by(region, Site, substrate, ibutton_id) %>% 
	arrange(date) %>% 
	mutate(temp_change = temperature - lag(temperature)) %>% 
	filter(!is.na(temp_change))

?lag

max_change <- all_change %>% 
	mutate(month = month(date)) %>%
	group_by(region, Site, substrate, ibutton_id, month) %>% 
	summarise_each(funs(max), temp_change) %>% 
	group_by(region, Site, substrate, ibutton_id) %>%
	summarise_each(funs(max), temp_change)
	
	
max_change %>% 
	ggplot(aes(x = Site, y = temp_change, color = substrate, fill = substrate)) + geom_boxplot() +
	scale_color_viridis(discrete = TRUE, begin = 0.5, end = 0.9) +
	scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 0.9)
	