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

all_surv <- left_join(surv_change, cobble2)

all_surv %>% 
	ungroup() %>% 
	mutate(height_level = ifelse(height_level == "high", "High shore", "Low shore")) %>% 
	mutate(treatment = case_when(treatment == "c" ~ "concrete",
															 treatment == "ct" ~ "bench control",
															 treatment == "v" ~ "vexar cobble")) %>% 
	ggplot(aes(x = reorder(treatment, -survive_change), y = survive_change, fill = treatment)) + geom_boxplot() + 
 facet_wrap( ~ height_level) + ylab("Percent change in abundance") +xlab("Treatment") + scale_fill_viridis_d(begin =0.3, end = 0.9)
ggsave("figures/cobble_transplant.png", height = 6, width = 9)

mod_surv <- lm(survive_change ~ treatment*height_level, data = all_surv)
summary(mod_surv)

unique(cobble_data$treatment)

cobble_data %>% 
	group_by(treatment, replicate, height_level) %>% 
	mutate(survivorship_loss = percent_alive - lag(percent_alive)) %>% View

