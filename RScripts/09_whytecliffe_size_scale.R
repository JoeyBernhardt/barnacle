

### whytecliffe size scale


wi <- read_csv("data-raw/whytecliffe_raw_buttons.csv")
ws <- read_csv("data-raw/whytecliffe_size_scale.csv")


View(wi2)
str(wi2)
unique(wi2$id)


wi2 <- wi %>% 
	unite(time, am, col = "time", remove =  TRUE, sep = " ") %>% 
	unite(Date, time, col = "date", remove = TRUE, sep = " ") %>% 
	mutate(date = dmy_hms(date)) %>% 
	gather(key = id, value = temperature, 2:27) 
	
ws2 <- ws %>% 
	mutate(size = dim1*dim2*dim3) %>%
	mutate(substrate = str_to_upper(substrate)) %>%
	mutate(substrate = ifelse(substrate == "BE", "B", substrate)) %>% 
	unite(col = id, remove = FALSE, sep = "", ID, substrate) 

all_ibs <- left_join(wi2, ws2, by = "id")

dh35 <- all_ibs %>% 
	filter(temperature > 35) %>% 
	mutate(dh_35 = temperature - 35) %>%
	group_by(id) %>% 
	summarise_each(funs(sum), dh_35)

dh1 <- left_join(dh35, ws2, by = "id")

View(dh1)

dh1 %>% 
	filter(size < 64000000) %>% 
	ggplot(aes(x = size, y = dh_35/12)) + geom_point() +
	ylab("Degree hours above 35°C") + xlab("Rock size (cm^3)")
ggsave("figures/degree_hours_size_scale.pdf", width = 6, height = 4)


thresholds <- all_ibs %>% 
	mutate(above20 = ifelse(temperature > 20, 1, 0),
				 above30 = ifelse(temperature > 30, 1, 0),
				 above35 = ifelse(temperature > 35, 1, 0),
				 above40 = ifelse(temperature > 40, 1, 0),
				 above45 = ifelse(temperature > 45, 1, 0)) %>% 
	group_by(id) %>% 
	summarise_each(funs(sum), contains("above"))

thres <- left_join(thresholds, ws2, by = "id")

thres_long <- thres %>% 
	gather(key = threshold, value = time_points, contains("above"))


thres_long %>% 
	filter(size < 64000000) %>% 
	ggplot(aes(x = size, y = time_points, color = verticalness)) + geom_point() +
	facet_wrap( ~ threshold, scales = "free") 


thres_long %>% 
	filter(size < 1400000) %>% 
	mutate(time = (time_points*5)/60) %>% 
	ggplot(aes(x = size, y = time)) + geom_point() +
	geom_smooth(method = "lm", color = "black") +
	facet_wrap( ~ threshold, scales = "free") +
	ylab("Time spent above threshold (hours)") + xlab("Rock size (cm^3)")
ggsave("figures/whytecliffe_size_scale_thresholds.pdf", width = 9, height = 6)


thres_long %>% 
mutate(substrate = ifelse(substrate == "C", "Cobble", "Bench")) %>% 
	mutate(time = (time_points*5)/60) %>% 
	group_by(substrate, threshold) %>% 
	summarise_each(funs(mean, std.error), time) %>% 
	ggplot(aes(x = substrate, y = time_mean)) + geom_point() +
	geom_errorbar(aes(ymin = time_mean - time_std.error, ymax = time_mean + time_std.error), width = 0.2) +
	geom_smooth(method = "lm", color = "black") +
	facet_wrap( ~ threshold, scales = "free") +
	ylab("Time spent above threshold (hours)") + xlab("Substrate")
ggsave("figures/whytecliffe_size_scale_thresholds_categories.pdf", width = 9, height = 6)


mod1 <- lm(log(above35 + 1) ~ log(size), data = filter(thres, verticalness == "h"))
summary(mod1)


dm <- all_ibs %>% 
	mutate(day = day(date)) %>%
	group_by(day, id) %>% 
	summarise_each(funs(max), temperature) %>% 
	group_by(id) %>% 
	summarise_each(funs(mean), temperature)

dm2 <- left_join(dm, ws2, by = "id")

dm2 %>% 
	filter(size < 64000000) %>% 
	ggplot(aes(x = size, y = temperature)) + geom_point() +
	ylab("Average daily max temperature (°C)") + xlab("Rock size (cm^3)")


dh <- all_ibs %>% 
	mutate(day = day(date)) %>%
	mutate(degrees_above = temperature - 13) %>%
	group_by(id, day) %>% 
	summarise_each(funs(sum), degrees_above) %>% 
	group_by(id) %>% 
	summarise_each(funs(mean), degrees_above)


dm3 <- left_join(dh, ws2, by = "id")

dm3 %>% 
	filter(size < 64000000) %>% 
	mutate(dm_height = degrees_above/height) %>% 
	ggplot(aes(x = size, y = dm_height, color = orientation)) + geom_point(size = 3) +
	geom_smooth(method = "lm", color = "black")
	