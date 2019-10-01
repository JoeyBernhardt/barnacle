### west van cobble transplant



library(tidyverse)
library(janitor)


cobble_data <- read_csv("data-raw/west_van_transplant.csv") %>% 
	clean_names()


cobble2 <- cobble_data %>% 
	filter(height_level != "mid") %>%
	mutate(total_counted = (type_1 + type_3 + type_2 + type_4)) %>% 
	mutate(percent_alive_calc = type_1/total_counted) %>%
	# filter(!is.na(percent_alive)) %>% View
	mutate(date = as.factor(date)) %>% 
	filter(date != "t2") %>% 
	mutate(unique_id = paste(treatment, height_level, replicate, sep = "_")) 



surv_change <- cobble2 %>% 
	select(treatment, replicate, height_level, percent_alive_calc, date, unique_id) %>% 
	group_by(unique_id, date) %>%
	summarise(mean_percent_alive = mean(percent_alive_calc)) %>% 
	ungroup() %>% 
	spread(key = date, value = mean_percent_alive) %>% 
	mutate(survive_change = t1 - t0) 

all_surv <- left_join(surv_change, cobble2) %>% 
	# filter(height_level == "low") %>% 
	ungroup() %>% 
	mutate(height_level = ifelse(height_level == "high", "High shore", "Low shore")) %>% 
	mutate(treatment = case_when(treatment == "c" ~ "thermally integrated",
															 treatment == "ct" ~ "bench control",
															 treatment == "v" ~ "thermally isolated")) %>% 
	# mutate(treatment = factor(treatment, levels = c("bench control", "concrete", "vexar cobble"))) %>% 
	filter(treatment != "bench control") %>% 
	distinct(survive_change, .keep_all = TRUE)

str(all_surv)

all_surv %>% 
	group_by(treatment, height_level) %>% 
	# summarise_each(funs(mean, std.error), survive_change) %>% 
	mutate(mean = mean(survive_change),
				 std.error = std.error(survive_change)) %>% 
	ggplot(aes(x = treatment, y = survive_change, fill = treatment, color = treatment)) + geom_jitter(width = 0.1) + 
	geom_point(aes(x = treatment, y = mean), size = 3) +
	geom_errorbar(aes(x = treatment, ymin = mean - std.error, ymax = mean + std.error), width = 0.1) +
 facet_wrap( ~ height_level) + ylab("Summer mortality \n (% change in abundance)") +
	xlab("Treatment") + scale_color_viridis_d(begin =0.3, end = 0.9) +
	theme(legend.position = "none")
ggsave("figures/cobble_transplant.png", height = 4, width = 8)

mod_surv <- aov(survive_change ~ treatment*height_level, data = all_surv)
summary(mod_surv)

mod_surv2 <- lm(survive_change ~ treatment, data = all_surv)
summary(mod_surv2)
TukeyHSD(mod_surv2)

unique(cobble_data$treatment)

cobble_data %>% 
	group_by(treatment, replicate, height_level) %>% 
	mutate(survivorship_loss = percent_alive - lag(percent_alive)) %>% View

temperatures <- read_csv("data-raw/cobble_transplant_west_van_ibuttons.csv")

temperatures %>% 
	ggplot(aes(x = date, y = daily_max_temperature, color = substrate)) + geom_point() + geom_line() +
	scale_color_viridis_d(begin =0.5, end = 0.9) +ylab("Daily max temperature") + xlab("Date")
ggsave("figures/cobble_transplant_temperature.png", height = 6, width = 9)

temperatures %>% 
	group_by(substrate) %>% 
	summarise_each(funs(mean, max), daily_max_temperature) %>% View


temperatures %>% 
	filter(daily_max_temperature > 38.5) %>% 
	group_by(substrate) %>% 
	tally()

temperatures %>% 
	filter(daily_max_temperature > 38.5) %>% 
	mutate(dd_38 = daily_max_temperature - 38.5) %>% 
	group_by(substrate) %>% 
	summarise(total = sum(dd_38))

temps_above <- temperatures %>% 
	# filter(daily_max_temperature > 38.5) %>% 
	mutate(dd_38 = daily_max_temperature - 38.5) %>% 
	mutate(dd_38 = ifelse(dd_38 < 0, 0, dd_38))

m <- lm(dd_38 ~ substrate, data = temps_above) 
visreg(m)
summary(m)
