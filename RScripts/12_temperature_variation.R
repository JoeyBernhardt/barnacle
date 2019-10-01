
daytime_em <- daytime_em %>% 
	filter(!ibutton_id %in% c("2B9CC6", "2B7OB7"))

sd_temps <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>%
	# filter(ibutton_id == "2BC24B", month_day == "7_11") %>% 
	group_by(site_rename, ibutton_id,substrate,region, month_day) %>% 
	summarise(sd_temp = sd(temperature)) %>% 
	filter(!is.na(sd_temp)) %>% 
	group_by(site_rename, ibutton_id,substrate,region) %>% 
	summarise(sd_temp = mean(sd_temp)) %>% 
	group_by(substrate, site_rename, region) %>% 
	summarise_each(funs(mean), sd_temp)

sd_temps_all <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>%
	# filter(ibutton_id == "2BC24B", month_day == "7_11") %>% 
	group_by(site_rename, ibutton_id,substrate,region) %>% 
	summarise(sd_temp_all = sd(temperature)) %>% 
	filter(!is.na(sd_temp_all)) %>% 
	ungroup() %>% 
	select(site_rename, sd_temp_all, substrate, region) %>% 
	group_by(substrate, site_rename, region) %>% 
	summarise_each(funs(mean), sd_temp_all)

emersion3b <- emersion3 %>%
	group_by(substrate, site_rename, region) %>% 
	summarise_each(funs(mean), hours_emersed)

emersion9 <- left_join(emersion3b, sd_temps) %>% 
	left_join(., sd_temps_all)


emersion9b <- emersion9 %>% 
	ungroup() %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	ungroup() %>% 
	filter(!is.na(sd_temp_all))

unique(emersion9b$region)	

emersion9b %>% 
ggplot(aes(x = sd_temp_all, y = hours_emersed, color = substrate, fill = substrate)) + geom_point(size = 3, alpha = 0.5) +
	geom_point(size = 3, shape = 1) + 
	geom_smooth(method = "lm") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("SD of temperatures during aerial exposure") + ylab("Daytime hours emersed (per day)") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/upper-limit-temperature-SD-all-summer.png", width = 8, height = 6)

library(lmodel2)
?lmodel2
emersion9c <- emersion9b %>% 
	filter(substrate == "Bench")

mod <- lmodel2(hours_emersed ~ sd_temp_all, data = emersion9b)
summary(mod)
mod$regression.results
cor(emersion9c$hours_emersed, emersion9c$sd_temp)

emersion9b %>% 
	ggplot(aes(x = sd_temp, y = hours_emersed, color = substrate)) + 
	geom_smooth(method = "lm") +
	geom_point(size = 3) +
	geom_point(size = 3, shape = 1) + 
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("SD of temperatures during aerial exposure") + ylab("Daytime hours emersed (per day)") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9)
ggsave("figures/upper-limit-temperature-SD.png", width = 8, height = 6)



emersion9b %>% 
	ggplot(aes(x = sd_temp, y = sd_temp_all, color = substrate)) + geom_point()


sum_dh <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>%
	group_by(site_rename, ibutton_id,substrate,region, month_day) %>% 
	filter(temperature > 38.5) %>% 
	mutate(dh_38.5 = temperature - 38.5) %>% 
	group_by(site_rename, ibutton_id,substrate,region) %>% 
	summarise(sum_dh = sum(dh_38.5)) %>% 
	group_by(site_rename,substrate,region) %>% 
	summarise(sum_dh = mean(sum_dh)) 

unique(sum_dh$site_rename)


emersion10 <- left_join(emersion3b, sum_dh) %>% 
	mutate(sum_dh = ifelse(is.na(sum_dh), 0, sum_dh))

emersion10 %>% 
	ggplot(aes(x = sum_dh, y = hours_emersed, color = substrate)) + geom_point() +
	geom_smooth(method = "lm")
