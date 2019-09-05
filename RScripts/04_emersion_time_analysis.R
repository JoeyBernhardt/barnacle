library(cowplot)
library(broom)
library(tidyverse)
library(viridis)
library(plotrix)
library(lubridate)


# read in data ------------------------------------------------------------


emersion <- read_csv("data-processed/emersion_time_upperlimits.csv")
emersion2 <- read_csv("data-processed/barnacle_emersion_times.csv") %>% 
	filter(substrate != "boulder")

emersion3 <- emersion2 %>% 
	mutate(site_rename = case_when(grepl("Ruckle", site) ~ "ruckle",
																 grepl("Welbury", site) ~ "welbury",
																 grepl("Sookes", site) ~ "sooke",
																 grepl("Sheep", site) ~ "sheep",
																 grepl("Eagle", site) ~ "eagle",
																 grepl("Crab", site) ~ "crab",
																 grepl("Toquart", site) ~ "toquart",
																 grepl("Uk", site) ~ "ukie",
																 grepl("Motel", site) ~ "sketchy",
																 grepl("Bios", site) ~ "biosphere",
																 grepl("Copper", site) ~ "copper",
																 grepl("Whyte", site) ~ "whytecliff",
																 grepl("28", site) ~ "28th",
																 grepl("Caul", site) ~ "caulfield",
																 grepl("Bam", site) ~ "bamfield")) %>% 
	mutate(date = str_replace(pattern = "Sept", replacement = "Sep", string = date)) %>% 
	mutate(date = mdy(date)) %>% 
	mutate(region = case_when(grepl("gulfislands", region) ~ "Gulf Islands",
														grepl("outer coast", region) ~ "Outer Coast",
														grepl("Vancouver", region) ~ "Vancouver"))
	



emersion_noBoulder <- emersion %>% 
	filter(substrate != "boulder")
# 
# emersion_noBoulder <- emersion_noBoulder %>% 
# 	mutate(emersion_time_hours = ((total/60)/29)) ## don't know why we divide by 29 here...look back into this
# 
# 
# emersion_noBoulder$Region <- ordered(emersion_noBoulder$Region, levels = c("gulf_islands", "vancouver", "outer_coast"))
# 
# 
# emersion_noBoulder %>% 
# 	ggplot(data = ., aes(x = Region, y = mean_height, fill = factor(substrate))) + geom_boxplot() + 
# 	ylab("height above MLLW, m") +
# 	theme(axis.text=element_text(size=16),
# 				axis.title=element_text(size=14,face="bold"))
# 
# 
# emersion_noBoulder %>% 
# 	ggplot(data = ., aes(x = Region, y = emersion_time_hours, fill = factor(substrate))) + geom_boxplot() + 
# 	ylab("Daily emersion time (hours)") +
# 	theme(axis.text=element_text(size=16),
# 				axis.title=element_text(size=14,face="bold")) +
# 	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
# ggsave("figures/emersion_time_boxplot.pdf", width = 6, height = 4)

emersion3 %>% 
	mutate(time_point = case_when(date < "2012-08-01" ~ "early_summer",
																date > "2012-08-01" ~ "late_summer")) %>% 
	ggplot(data = ., aes(x = region, y = hours_emersed, fill = factor(substrate))) + geom_boxplot() + 
	ylab("Daily emersion time (hours)") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) + facet_wrap( ~ time_point)
ggsave("figures/emersion_time_boxplot2.pdf", width = 6, height = 4)
ggsave("figures/emersion_time_boxplot3.pdf", width = 10, height = 4)


late_summer <- emersion3 %>% 
	mutate(time_point = case_when(date < "2012-08-01" ~ "early_summer",
																date > "2012-08-01" ~ "late_summer")) %>% 
	filter(time_point == "late_summer")


ls_means <- late_summer %>% 
	group_by(region, site_rename, substrate) %>% 
	summarise_each(funs(mean), hours_emersed)


mod1 <- lm(hours_emersed ~ substrate*region, data = ls_means)
summary(mod1)
anova(mod1)

library(visreg)
visreg(fit = mod1)



emersion3 %>% 
	ggplot(aes(x = date, y = hours_emersed)) + geom_point() +
	facet_wrap( ~ site_rename + substrate, scales = "free")
ggsave("figures/emersion_times_summer.pdf", width = 12, height = 12)

### bring in temperature data
unique(emersion3$site_rename)

temps <- read_csv("data-processed/all_temps_w_tides.csv")
daytime_em <- temps %>% 
	filter(emersed == "emersed" & daytime == "daytime") %>% 
	mutate(region = ifelse(region == "Saltspring", "Gulf Islands", region)) %>% 
	mutate(region = factor(region, levels = c("Outercoast", "Gulf Islands", "Vancouver")))


dh_40 <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	filter(temperature > 40) %>% 
	mutate(dh_35 = temperature - 40) %>% 
	group_by(site_rename, ibutton_id,substrate, month_day) %>% 
	summarise_each(funs(sum), dh_35) %>% 
	group_by(site_rename, substrate) %>% 
	summarise_each(funs(min, mean, max), dh_35)

dh_38.5 <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	filter(temperature > 38.5) %>% 
	mutate(dh_35 = temperature - 38.5) %>% 
	group_by(site_rename, ibutton_id,substrate, month_day) %>% 
	summarise_each(funs(sum), dh_35) %>% 
	group_by(site_rename, substrate) %>% 
	summarise_each(funs(min, mean, max, sum), dh_35)

dh_40 <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	filter(temperature > 40) %>% 
	mutate(dh_40 = temperature - 40) %>% 
	group_by(site_rename, ibutton_id,substrate, month_day) %>% 
	summarise_each(funs(sum), dh_40) %>% 
	group_by(site_rename, substrate) %>% 
	summarise_each(funs(min, mean, max, sum), dh_40)

dh_42 <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	filter(temperature > 42) %>% 
	mutate(dh_42 = temperature - 42) %>% 
	group_by(site_rename, ibutton_id,substrate, month_day) %>% 
	summarise_each(funs(sum), dh_42) %>% 
	group_by(site_rename, substrate) %>% 
	summarise_each(funs(min, mean, max, sum), dh_42)


dh_35 <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	filter(temperature > 35) %>% 
	mutate(dh_35 = temperature - 35) %>% 
	group_by(site_rename, ibutton_id,substrate, month_day) %>% 
	summarise_each(funs(sum), dh_35) %>% 
	group_by(site_rename, substrate) %>% 
	summarise_each(funs(min, mean, max, sum), dh_35)


daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	# filter(temperature > 38.5) %>% 
	mutate(dh_35 = temperature - 38.5) %>% 
	mutate(dh_35 = ifelse(dh_35 < 0, 0, dh_35)) %>% 
	group_by(month_day, region, Site, substrate) %>%
	summarise(sum_dh = sum(dh_35)) %>% 
	filter(sum_dh > 0) %>% 
	ggplot(aes(x = sum_dh, fill = substrate)) + geom_density(alpha = 0.5) +
	facet_wrap(~ Site, scales = "free")

ctmax <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	# filter(temperature > 38.5) %>% 
	mutate(dh_35 = temperature - 35) %>% 
	mutate(dh_35 = ifelse(dh_35 < 0, 0, dh_35)) %>% 
	group_by(month_day, region, Site, substrate, ibutton_id) %>%
	summarise(sum_dh = sum(dh_35)) %>% 
	group_by(Site, region, substrate) %>% 
	summarise_each(funs(mean, std.error), sum_dh)

ctmax1 <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	# filter(temperature > 38.5) %>% 
	mutate(dh_35 = temperature - 35) %>% 
	mutate(dh_35 = ifelse(dh_35 < 0, 0, dh_35)) %>% 
	group_by(month_day, region, Site, substrate, ibutton_id) %>%
	summarise(sum_dh = sum(dh_35)) %>% 
	group_by(Site, region, substrate) %>% 
	summarise_each(funs(mean, std.error), sum_dh)

mod3 <- lm(mean ~ substrate, data = ctmax1) 
summary(mod3)

ctmax_raw <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	# filter(temperature > 38.5) %>% 
	mutate(dh_35 = temperature - 35) %>% 
	mutate(dh_35 = ifelse(dh_35 < 0, 0, dh_35)) %>% 
	group_by(month_day, region, Site, substrate, ibutton_id) %>% 
	summarise(sum_dh = sum(dh_35)) %>% 
	group_by(region, Site, substrate, ibutton_id) %>% 
	top_n(n = 40, wt = month_day) 

ctmax_raw %>% 
	group_by(region, Site, substrate, ibutton_id) %>% 
	tally() %>% View

### effect of substrate size on temperatures
mod1 <- lm(sum_dh ~ substrate*Site, data = ctmax_raw) 
summary(mod1)

library(lme4)

mod2 <- lmer(sum_dh ~ substrate +  (1|region) + (1|Site), data = ctmax_raw)
summary(mod2)

	# filter(sum_dh > 0) %>% 
	ggplot() + 
	geom_point(aes(x = substrate, y = sum_dh, color = substrate), data = ctmax_raw, position= position_jitter(width = 0.1), alpha = 0.4 ) + 
	# geom_point(alpha = 0.5, position = position_jitter(width = 0.1)) +
	geom_pointrange(aes(x = substrate, y = mean, ymin = mean - std.error, ymax = mean + std.error, color = substrate), data = ctmax, size = 1) +
		geom_point(aes(x = substrate, y = mean), data = ctmax, size = 4, color = "black", shape = 1) +
		# geom_errorbar(aes(x = substrate, ymax = mean + std.error, ymin = mean - std.error), width = 0.1, data = ctmax) +
	facet_wrap(region ~ Site, scales = "free") +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	ylab("Daily degree hours above CTmax (June - August 2012)") + xlab("")
	
	ggplot() + 
		# geom_point(aes(x = substrate, y = sum_dh, color = substrate), data = ctmax_raw, position= position_jitter(width = 0.1), alpha = 0.4 ) + 
		# geom_point(alpha = 0.5, position = position_jitter(width = 0.1)) +
		geom_pointrange(aes(x = substrate, y = mean, ymin = mean - std.error, ymax = mean + std.error, color = substrate), 
										data = ctmax, size = 1) +
		# geom_point(aes(x = substrate, y = mean), data = ctmax, size = 5, color = "black", shape = 1) +
		# geom_errorbar(aes(x = substrate, ymax = mean + std.error, ymin = mean - std.error), width = 0.1, data = ctmax) +
		facet_wrap(region ~ Site, scales = "free") +
		scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
		ylab("Daily degree hours above CTmax (June - August 2012)") + xlab("")
	ggsave("figures/degree-hours-ctmax-region.png", width = 8, height = 8)









daytime_em %>%
	mutate(minute = 1) %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	mutate(above20 = ifelse(temperature > 20, 1, 0)) %>% 
	mutate(above25 = ifelse(temperature > 25, 1, 0)) %>% 
	mutate(above30 = ifelse(temperature > 30, 1, 0)) %>% 
	mutate(above35 = ifelse(temperature > 35, 1, 0)) %>% 
	mutate(above40 = ifelse(temperature > 40, 1, 0)) %>% 
	mutate(above45 = ifelse(temperature > 45, 1, 0)) %>% 
	gather(key = threshold, value = hours, 19:24) %>% 
	group_by(site_rename, substrate, month_day, ibutton_id, region, threshold) %>% 
	summarise_each(funs(sum), hours) %>% 
	ggplot(aes(x = hours, color = substrate, fill = substrate, group = substrate)) + geom_density() +
	facet_wrap( ~ threshold, scales = "free")

days_35 <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	mutate(above_35 = ifelse(temperature > 35, 1, 0)) %>% 
	group_by(site_rename, ibutton_id, substrate, month_day, region) %>% 
	summarise_each(funs(sum), above_35) %>% 
	mutate(day_above_35 = ifelse(above_35>0, 1, 0)) %>% 
	group_by(site_rename, substrate, ibutton_id, region) %>% 
	summarise_each(funs(sum), day_above_35) %>% 
	group_by(site_rename, substrate) %>% 
	summarise_each(funs(mean), day_above_35) 




daytime_summary <- daytime_em %>% 
	mutate(region = as.character(region)) %>% 
	group_by(site_rename, substrate) %>% 
	summarise_each(funs(mean, max, median), temperature) 
	
	View(daytime_summary)

setdiff(unique(emersion4$site_rename),unique(daytime_summary$site_rename))

daytime_max <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	group_by(site_rename, substrate, month_day) %>% 
	summarise_each(funs(max), temperature) %>% 
	group_by(site_rename, substrate) %>% 
	summarise_each(funs(mean), temperature)

dh_35 %>% View


emersion4 <- left_join(emersion3, daytime_summary)
emersion5 <- left_join(emersion3, daytime_max)
emersion6 <- left_join(emersion3, dh_38.5)
emersion6b <- left_join(emersion3, dh_40)
emersion6c <- left_join(emersion3, dh_42)
emersion7 <- left_join(emersion3, dh_35)
emersion8 <- left_join(emersion3, days_35)

time_above_thresholds <- daytime_em %>%
	mutate(minute = 1) %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	mutate(above20 = ifelse(temperature > 20, 1, 0)) %>% 
	mutate(above25 = ifelse(temperature > 25, 1, 0)) %>% 
	mutate(above30 = ifelse(temperature > 30, 1, 0)) %>% 
	mutate(above35 = ifelse(temperature > 35, 1, 0)) %>% 
	mutate(above40 = ifelse(temperature > 40, 1, 0)) %>% 
	mutate(above45 = ifelse(temperature > 45, 1, 0)) %>% 
	gather(key = threshold, value = hours, 19:24) %>% 
	group_by(site_rename, substrate, month_day, ibutton_id, region, threshold) %>% 
	summarise_each(funs(sum), hours) %>%
	group_by(site_rename, substrate, ibutton_id, region, threshold) %>% 
	summarise_each(funs(mean), hours) %>% 
	group_by(substrate, threshold) %>%
	summarise_each(funs(mean, std.error), hours) 


min.mean.sd.max <- function(x) {
	r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
	names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
	r
}


time_above_thresholds %>% 
	ggplot(aes(x = substrate, y = hours_mean, color = substrate)) +
	geom_point() +
	geom_errorbar(aes(ymin = hours_mean - hours_std.error, ymax = hours_mean + hours_std.error), width = 0.2) +
	facet_wrap( ~ threshold, scales = "free") +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	ylab("Average hours per day above threshold") + xlab("")
ggsave("figures/time_above_temp_thresholds.pdf", width = 7, height = 4)


### let's calculate degree hours, 13C is the average min temperature. 


daytime_em %>%
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	mutate(degrees_above = temperature - 13) %>% 
	group_by(region, site_rename, substrate, month_day, ibutton_id) %>% 
	summarise_each(funs(sum), degrees_above) %>% 
	group_by(region, site_rename, substrate, ibutton_id) %>% 
	summarise_each(funs(mean), degrees_above) %>% 
	ggplot(aes(x = reorder(site_rename, degrees_above, FUN = "mean", na.rm = TRUE), y = degrees_above, color = substrate)) + geom_boxplot() +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	ylab("Degree hours per day above 13°C") + xlab("Site")
ggsave("figures/degree_hours.pdf", width = 10, height = 5)

daytime_em %>%
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	filter(temperature > 38.6) %>% 
	mutate(degrees_above = temperature - 38.6) %>% 
	group_by(region, site_rename, substrate, month_day, ibutton_id) %>% 
	summarise_each(funs(sum), degrees_above) %>% 
	# group_by(region, site_rename, substrate) %>% 
	group_by(region, substrate) %>% 
	summarise_each(funs(mean, std.error), degrees_above) %>% 
	ggplot(aes(x = reorder(region, mean, FUN = "mean", na.rm = TRUE), y = mean, color = substrate)) + geom_point() +
	geom_errorbar(aes(ymin = mean - std.error, ymax =  mean + std.error), width = 0.2) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) + 
	ylab("Degree hours per day above 38.6°C") + xlab("Site") 
ggsave("figures/degree_hours_38_6_point.pdf", width = 6, height = 5)
ggsave("figures/degree_hours_35_point.pdf", width = 6, height = 5)
ggsave("figures/degree_hours_38.6_point_site.pdf", width = 10, height = 5)




## average daytime emersion temperature
emersion4 %>% 
	filter(date > "2012-08-1") %>% 
	# filter(grepl("Ukie", site)) %>% 
	ggplot(aes(x = mean, y = hours_emersed, color = site_rename)) + geom_point(size = 5) +
	facet_wrap( ~ substrate + region)

emersion4 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	filter(date > "2012-08-1") %>% 
	ggplot(aes(x = mean, y = hours_emersed)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	facet_wrap( ~ substrate) + geom_smooth(method = "lm", color = "black") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Average daytime rock temperature (°C)") + ylab("Daytime hours emersed (per day)")
ggsave("figures/emersion_hours_average_daytime_temperature.pdf", width = 8, height = 4)



emersion4 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	filter(date > "2012-08-1") %>% 
	ggplot(aes(x = mean, y = hours_emersed, color = substrate, fill = substrate)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	# facet_wrap( ~ substrate) +
	geom_smooth(method = "lm") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Average daytime rock temperature (°C)") + ylab("Daytime hours emersed (per day)") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/emersion_hours_average_daytime_temp_color.pdf", width = 6, height = 4)


emersion4 %>% 
	filter(date > "2012-08-1") %>%
	group_by(substrate) %>% 
	do(tidy(lm(hours_emersed ~ temperature_mean, data = .), conf.int = TRUE)) %>% View

### average daily max
emersion5 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	filter(date > "2012-08-1") %>%
	ggplot(aes(x = temperature, y = hours_emersed)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	facet_wrap( ~ substrate) + geom_smooth(method = "lm", color = "black") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Average daily max rock temperature (°C)") + ylab("Daytime hours emersed (per day)")
ggsave("figures/emersion_hours_temperature.pdf", width = 8, height = 4)


emersion5 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	filter(date > "2012-08-1") %>% 
	ggplot(aes(x = temperature, y = hours_emersed, color = substrate, fill = substrate)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	# facet_wrap( ~ substrate) +
	geom_smooth(method = "lm") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Average daily max rock temperature (°C)") + ylab("Daytime hours emersed (per day)") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/emersion_hours_daily_max_temp_color.pdf", width = 6, height = 4)

### degree hours above 40
emersion6 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	filter(date > "2012-08-1") %>% 
	ggplot(aes(x = mean, y = hours_emersed, color = substrate, fill = substrate)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	facet_wrap( ~ region) +
	geom_smooth(method = "lm") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Average daily degree hours above 38.5°C") + ylab("Daytime hours emersed (per day)") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/emersion_hours_degree_hours_color_40.pdf", width = 6, height = 4)
ggsave("figures/emersion_hours_degree_hours_color_38_5.png", width = 6, height = 4)

emersion7 %>% 
	mutate(sum = ifelse(is.na(sum), 0.000001, sum)) %>% 
	# filter(sum > 0.5) %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	filter(date > "2012-08-1") %>% 
	ggplot(aes(x = mean, y = hours_emersed)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	# scale_x_log10() +
	# facet_wrap( ~ substrate) +
	# geom_smooth() +
	# facet_wrap( ~ region, scales = "free") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Mean daily degree hours above 35°C") + ylab("Daytime hours emersed (per day)") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)


### degree hours above 35
emersion7 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	filter(date > "2012-08-1") %>% 
	ggplot(aes(x = dh_35_mean, y = hours_emersed, color = substrate, fill = substrate)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	# facet_wrap( ~ substrate) +
	geom_smooth(method = "lm") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Average daily degree hours above 35°C") + ylab("Daytime hours emersed (per day)") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/emersion_hours_degree_hours_color_35.pdf", width = 6, height = 4)


### days above 35
emersion8 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	filter(date > "2012-08-1") %>% 
	ggplot(aes(x = day_above_35, y = hours_emersed, color = substrate, fill = substrate)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	# facet_wrap( ~ substrate) +
	geom_smooth(method = "lm") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Days above 35°C") + ylab("Daytime hours emersed (per day)") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/emersion_hours_days_above_35.pdf", width = 6, height = 4)



emersion5 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	# filter(time_point == "End of summer") %>% 
	group_by(substrate, time_point, region, site_rename) %>% 
	summarise_each(funs(mean), hours_emersed) %>% 
	group_by(substrate, time_point, region) %>% 
	summarise_each(funs(mean, std.error), hours_emersed) %>% 
	ggplot(aes(x = substrate, y = mean, color = substrate)) + 
	geom_point(size = 3) + geom_errorbar(aes(ymin = mean - std.error,
																	 ymax = mean + std.error), width = 0.2) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	facet_wrap( ~ region + time_point, nrow = 3, ncol = 2) +
xlab("") + ylab("Daytime hours emersed (per day)") +
	theme(strip.background = element_rect(colour="white", fill="white")) 
ggsave("figures/emersion_hours_region_color2.pdf", width = 6, height = 8)
ggsave("figures/emersion_hours_region_color_point.pdf", width = 6, height = 8)


emersion5 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	# filter(time_point == "End of summer") %>% 
	ggplot(aes(x = substrate, y = hours_emersed, color = substrate)) + 
	geom_boxplot() +
	geom_point(position = position_jitter(width = 0.2)) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	facet_grid(region ~ time_point) +
	ylab("Upper vertical limit \n (Daytime hours emersed per day)") + 
	theme(legend.position = "none")
ggsave("figures/emersion_hours_region_color3.pdf", width = 6, height = 8)
	




emersion5 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	# filter(time_point == "End of summer") %>% 
	group_by(substrate, time_point, region, site_rename) %>% 
	summarise_each(funs(mean), x_max_tide_height) %>% 
	group_by(substrate, time_point, region) %>% 
	summarise_each(funs(mean, std.error), x_max_tide_height) %>% 
	ggplot(aes(x = substrate, y = x_max_tide_height_mean, color = substrate)) + 
	geom_point(size = 3) + geom_errorbar(aes(ymin = x_max_tide_height_mean - x_max_tide_height_std.error,
																					 ymax = x_max_tide_height_mean + x_max_tide_height_std.error), width = 0.2) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	facet_wrap( ~ region + time_point, nrow = 3, ncol = 2) +
	xlab("") + ylab("Proportion of max tide height") +
	theme(strip.background = element_rect(colour="white", fill="white"))
ggsave("figures/percent_tide_height_region_color_point.pdf", width = 6, height = 8)




emersion5 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	# filter(time_point == "End of summer") %>% 
	group_by(substrate, time_point, region, site_rename) %>% 
	summarise_each(funs(mean), height_above_mllw) %>% 
	group_by(substrate, time_point, region) %>% 
	summarise_each(funs(mean, std.error), height_above_mllw) %>% 
	ggplot(aes(x = substrate, y = height_above_mllw_mean, color = substrate)) + 
	geom_point(size = 3) + geom_errorbar(aes(ymin = height_above_mllw_mean - height_above_mllw_std.error,
																					 ymax = height_above_mllw_mean + height_above_mllw_std.error), width = 0.2) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	facet_wrap( ~ region + time_point, nrow = 3, ncol = 2) +
	xlab("") + ylab("Height above MLLW (m)") +
	theme(strip.background = element_rect(colour="white", fill="white"))
ggsave("figures/height_mllw_region_color_point.pdf", width = 6, height = 8)



emersion5 %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	mutate(time_point = case_when(date > "2012-08-1" ~ "End of summer",
																date < "2012-08-1" ~ "Beginning of summer")) %>% 
	filter(date > "2012-08-1") %>%
	ggplot(aes(x = temperature, y = height_above_mllw)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	facet_wrap( ~ substrate, scales = "free") + geom_smooth(method = "lm", color = "black") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("Average daily max temperature (°C)") + ylab("Height above MLLW (m)")
ggsave("figures/height_temperature.pdf", width = 8, height = 4)


emersion5 %>% 
	filter(date > "2012-08-1") %>%
	group_by(substrate) %>% 
	do(tidy(lm(hours_emersed ~ temperature, data = .), conf.int = TRUE)) %>% View



unique(emersion4$temperature_mean)

emersion_noBoulder %>% 
	ggplot(aes(x = substrate, y = emersion_time_hours, fill = factor(substrate))) + geom_bar(stat = "identity") + 
	ylab("hours of emersion time per day") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) + facet_wrap( ~ Site) +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/emersion_time_barcharts.pdf", width = 12, height = 8)


emersion <- emersion_noBoulder %>% 
	mutate(Date = str_replace(Date, pattern = "Sept", replacement = "Sep")) %>% 
	mutate(date = mdy(Date)) 

emersion %>% 
	ggplot(aes(x = date, y = mean_height)) + geom_point() +
	facet_wrap( ~ Site + substrate, scales = "free")


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
	unite(col = "site_substrate", remove = FALSE, isite, substrate) %>% 
	filter(!is.na(isite)) %>% 
	select(-Site)



	
all4 <- read_csv("data-processed/all_ibutton_data.csv")



all5 <- all4 %>% 
	mutate(day = date(date)) %>% 
	group_by(substrate, Site, region, ibutton_id, day) %>% 
	summarise_each(funs(max, mean), temperature) %>% 
	group_by(substrate, Site, region, ibutton_id) %>% 
	summarise_each(funs(mean, max), max) %>% 
	unite(col = "site_substrate", remove = FALSE, Site, substrate)

all5 %>% 
	ggplot(aes(x = Site, y = max, color = substrate)) + geom_point()

em2 <- left_join(em, all5, by = c("substrate", "site_substrate"))	

em2 %>% 
	# filter(isite == "Sheepfarm") %>% 
	ggplot(aes(x = max, y = emersion_time_hours, color = substrate.x)) + geom_point() +
	facet_wrap(~ substrate) + geom_smooth(method = "lm") +
	xlab("Mean daily max temperature (°C)") + ylab("Daily emersion time (hours)") +
	scale_color_viridis(discrete = TRUE, begin = 0.5, end = 0.9) 
ggsave("figures/emersion_time_temp.pdf")

mod2 <- lm(emersion_time_hours ~ substrate + Site, data = em2)
mod3 <- lm(emersion_time_hours ~ substrate + Site + temperature_max_mean, data = em2)

summary(mod2)
anova(mod2)

AIC(mod2, mod3)

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
	filter(Site == "Sheepfarm") %>% 
	filter(date > "2012-08-01", date < "2012-08-29") 
ful3 <- ful2 %>% 
	mutate(height2 = height*2.3) %>% 
	filter(date > "2012-08-01", date < "2012-08-29") %>% 
	mutate(height_m = height*0.3048) %>% 
	mutate(height_s = height_m*5) %>% 
	mutate(hour = hour(day)) %>% 
	mutate(daytime = ifelse(hour > 7 & hour < 19, "daytime", "nighttime"))
gan3 <- gan2 %>% 
	filter(day > "2012-08-01", day < "2012-08-29") %>% 
	mutate(height2 = height*2.3) %>% 
	mutate(height_m = height*0.3048) %>% 
	mutate(height_s = height_m*5)
	

ggplot() + geom_point(aes(color = substrate, x = hour_day, y = temperature), data = sheep) +
	geom_line(aes(color = substrate, x = hour_day, y = temperature, group = ibutton_id), data = sheep) +
	geom_point(data = ful3, aes(x = day, y = height_s, color = daytime), size = 0.2) +
	scale_color_brewer(type = "div", palette = 4) +
	geom_hline(yintercept = 2.756746*5, color = "black") +
	geom_hline(yintercept = 2.636746*5, color = "black") +
	geom_hline(yintercept = 2.946746*5, color = "black") +
	geom_hline(yintercept = 2.116746*5, color = "grey") +
	geom_hline(yintercept = 2.076746*5, color = "grey") +
	geom_hline(yintercept = 2.036746*5, color = "grey")
ggsave("figures/sheepfarm_temps.pdf", width = 20, height = 4)
ggsave("figures/welbury_temps.pdf", width = 20, height = 4)
ggsave("figures/sookes_temps.pdf", width = 20, height = 4)

ful4 <- ful2 %>% 
	filter(day > "2012-08-05", day < "2012-08-07")


### ok for the sheepfarm ibutton data, let's try to extract only the points where the water is below the ibutton and it's daytime.



sheep_date <- sheep %>% 
	rename(day_merge = date)

ful_date <- ful3 %>% 
	rename(day_merge = day)


all_sheep <- left_join(sheep_date, ful_date, by = "day_merge")

all_sheep2 <- all_sheep %>% 
	# filter(daytime == "daytime") %>% 
	mutate(emersed = NA) %>% 
	mutate(emersed = case_when(substrate == "cobble" & height_m < 2.036746 ~ "emersed",
														 substrate == "bench" & height_m < 2.636746 ~ "emersed",
														 substrate == "cobble" & height_m > 2.036746 ~ "submerged",
														 substrate == "bench" & height_m > 2.636746 ~ "emersed")) 

ggplot() + geom_point(aes(color = emersed, x = hour_day, y = temperature, shape = substrate), data = all_sheep2) +
	geom_line(aes(color = emersed, x = hour_day, y = temperature, group = ibutton_id), data = all_sheep2) +
	# geom_point(data = all_sheep2, aes(x = hour_day, y = height_s, color = daytime), size = 0.2) +
	# scale_color_brewer(type = "div") +
	# scale_color_manual(type = "div") +
	geom_hline(yintercept = 2.756746*5, color = "black") +
	geom_hline(yintercept = 2.636746*5, color = "black") +
	geom_hline(yintercept = 2.946746*5, color = "black") +
	geom_hline(yintercept = 2.116746*5, color = "grey") +
	geom_hline(yintercept = 2.076746*5, color = "grey") +
	geom_hline(yintercept = 2.036746*5, color = "grey")
ggsave("figures/sheepfarm_temps_emersion.pdf", width = 20, height = 4)



sheep_emersed <- all_sheep2 %>% 
	filter(emersed == "emersed", daytime == "daytime") %>% 
	mutate(above20 = ifelse(temperature > 20, 1, 0),
				 above30 = ifelse(temperature > 30, 1, 0),
				 above0 = ifelse(temperature > 0, 1, 0))


sheep_dh <- sheep_emersed %>% 
	group_by(substrate, ibutton_id) %>% 
	summarise_each(funs(sum), above20, above30) %>% 
	mutate(dh20 = ifelse(substrate == "cobble", above20/127, above20/306)) %>% 
	mutate(dh30 = ifelse(substrate == "cobble", above30/127, above30/306))


sheep_emersed %>% 
	filter(date > "2012-08-01", date < "2012-08-24") %>% 
	ggplot(aes(x = temperature, color = substrate, fill = substrate)) + geom_density(alpha = 0.4) +
	scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.7) +
	scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.7) +
	xlab("Temperature (°C)") + ylab("Density")
ggsave("figures/sheepfarm_density.pdf", width = 7, height = 5)

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


max_change <- all_change %>% 
	mutate(month = month(date)) %>%
	group_by(region, Site, substrate, ibutton_id, month) %>% 
	summarise_each(funs(max), temp_change) %>% 
	group_by(region, Site, substrate, ibutton_id) %>%
	summarise_each(funs(max), temp_change)
	
	
max_change %>% 
	ggplot(aes(x = Site, y = temp_change, color = substrate, fill = substrate)) + geom_boxplot() +
	scale_color_viridis(discrete = TRUE, begin = 0.5, end = 0.9) +
	scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 0.9) + ylab("Maximum heating rate (°C/hour)")
ggsave("figures/heating_rate.png", width = 11, height = 4)

mod2 <- lm(temp_change ~ substrate*Site, data = max_change)
summary(mod2)	


library(lme4)

modm <- lmer(temp_change ~ substrate + (1|Site), data = max_change)
summary(modm)
anova(modm)


max_change %>% 
	ggplot(aes(x = substrate, y = temp_change, color = substrate, fill = substrate)) + geom_boxplot() +
	scale_color_viridis(discrete = TRUE, begin = 0.5, end = 0.9) +
	scale_fill_viridis(discrete = TRUE, begin = 0.5, end = 0.9) + ylab("Maximum heating rate (°C/hour)")
