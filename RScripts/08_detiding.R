
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
															 Site == "Whyte" ~ "atkinson")) %>% 
	rename(site_id = site)



atkinson_ibutton <- all_i2 %>% 
	filter(tide_site == "atkinson") %>% 
	rename(site_id = site)

atkinson_tides <- all_tides %>% 
	filter(site == "atkinson")

all_at <- left_join(atkinson_ibutton, atkinson_tides, by = "date") %>% 
	filter(!is.na(height))

all_tides2 <- all_tides %>% 
	mutate(tide_site = site)

all_temps_tides <- left_join(all_i2, all_tides2, by = c("date", "tide_site")) %>% 
	filter(!is.na(height))



ib_heights <- read_csv("data-processed/ibutton_heights_summary.csv")

View(ib_heights)

all_temps_tides2 <- left_join(all_temps_tides, ib_heights, by = c("Site" = "tide_site", "substrate")) ### ok now we have the temp records and the ibutton heights


all_temps_tides3 %>% 
	filter(emersed == "emersed") %>% 
	filter(Site == "Eagle") %>% 
	group_by(substrate) %>% 
	tally()

all_temps_tides3 <- all_temps_tides2 %>% 
	mutate(emersed = ifelse(height < height_above_mllw_min, "emersed", "submerged")) %>% 
	mutate(hour = hour(date)) %>%
	mutate(daytime = ifelse(hour > 7 & hour < 19, "daytime", "nighttime"))

all_temps_tides3 %>% 
	filter(emersed == "emersed" & daytime == "daytime") %>% 
	ggplot(aes(x = temperature, color = substrate, fill = substrate)) + geom_density(alpha = 0.4) +
	scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.7) +
	scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.7) +
	xlab("Temperature (°C)") + ylab("Density") +
	facet_wrap( ~ Site)
ggsave("figures/all_sites_temp_density.pdf", width = 10, height = 8)

all_temps_tides3 %>% 
	filter(emersed == "emersed" & daytime == "daytime" & Site == "Sheepfarm") %>% 
	filter(date > "2012-08-01", date < "2012-08-24") %>% 
	ggplot(aes(x = temperature, color = substrate, fill = substrate)) + geom_density(alpha = 0.4) +
	scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.7) +
	scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.7) +
	xlab("Temperature (°C)") + ylab("Density")
ggsave("figures/sheepfarm_density2.pdf", width = 7, height = 5)


daytime_em <- all_temps_tides3 %>% 
	filter(emersed == "emersed" & daytime == "daytime") 

daytime_summary <- daytime_em %>% 
	mutate(region = as.character(region)) %>% 
	group_by(region, Site, substrate, ibutton_id) %>% 
	summarise_each(funs(mean, max, median), temperature) %>% 
	ungroup()

daytime_summary %>% 
	group_by(region) %>% 
	do(tidy(lm(temperature_mean ~ substrate, data = .), conf.int = TRUE)) 

mod <- lm(temperature_mean ~ Site*substrate, data = daytime_summary) 
anova(mod)
summary(mod)

daytime_summary %>% 
	ggplot(aes(x = Site, y = temperature_mean, color = substrate, fill = substrate)) + geom_boxplot() +
	scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.7) +
	scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.7) + facet_wrap( ~ region)
ggsave("figures/daytime_emersion_temps.pdf", width = 10, height = 6)

all_temps_tides3 %>% 
	filter(Site == "Eagle") %>% 
	ggplot(aes(x = date, y = temperature, color = substrate, group = ibutton_id)) + geom_point() + geom_line() +
	geom_point(shape = 1, color = "black", aes(x = date, y = temperature), data = filter(all_temps_tides3, Site == "Eagle", emersed == "emersed", daytime == "daytime"))
ggsave("figures/eagle_temps_time.pdf", width = 20, height = 4)
