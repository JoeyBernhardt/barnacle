



ctmax_2 <- daytime_em %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	unite(month_day, month, day) %>% 
	filter(temperature > 38.5) %>% 
	mutate(dh_35 = temperature - 38.5) %>% 
	mutate(dh_35 = ifelse(dh_35 < 0, 0, dh_35)) %>% 
	group_by(month_day, region, site_rename, substrate, ibutton_id) %>%
	summarise(sum_dh = sum(dh_35)) %>% 
	group_by(substrate, region, site_rename) %>% 
	summarise_each(funs(mean, std.error), sum_dh) %>% 
	ungroup()


em5b %>% 
	ggplot(aes(x = substrate, y = mean_hours, color = substrate)) + 
	geom_point(position = position_jitter(width = 0.2)) +
	geom_errorbar(aes(x = substrate, ymin = mean_hours_reg - se_hours_reg, ymax = mean_hours_reg + se_hours_reg),
								width = 0.1, color = 'black') +
	geom_point(aes(x = substrate, y = mean_hours_reg), size = 3) +
	geom_point(aes(x = substrate, y = mean_hours_reg), size = 3, shape = 1, color = "black") +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	facet_wrap(~ region) +
	ylab("Upper vertical limit \n (Daytime hours emersed per day)") + 
	theme(legend.position = "none")


em6 <- em5b %>% 
	ungroup() %>% 
	mutate(substrate = ifelse(substrate == "Cobble", "cobble", "bench"))

View(em6)
View(ctmax_2)




em7 <- dplyr::full_join(ctmax_2, em6)

em7 %>% 
	ggplot(aes(x = mean, y = mean_hours, color = substrate)) + geom_point() +
	geom_smooth(method = "lm") + ylab("Upper vertical limit") + xlab("Daily hours above CTmax")
