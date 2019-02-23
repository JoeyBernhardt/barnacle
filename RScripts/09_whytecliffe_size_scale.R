
library(tidyverse)
library(lubridate)
library(cowplot)
library(plotrix)
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




### Do cobbles heat up faster?
all_ibs %>% 
	# filter(id %in% c("17C", "11B", "1B", "9C")) %>% 
	ggplot(aes(x = date, y = temperature, color = height, group = id)) + geom_line() +
	facet_grid(verticalness ~ substrate) + scale_color_viridis_c()
ggsave("figures/whytecliffe-temperatures.png", width = 20, height = 15)

all_ibs %>% 
	filter(temperature > 20) %>% 
	# filter(date > ymd_hms("2012-08-31 01:56:00")) %>% 
	# filter(date < ymd_hms("2012-09-01 09:56:00")) %>% 
	# filter(id %in% c("17C", "11B", "1B", "9C")) %>% 
	ggplot(aes(x = date, y = temperature, color = substrate, group = id)) + geom_point(size = 0.5)



# find the max heating rate -----------------------------------------------

C17 <- all_ibs %>% 
	mutate(start_time = min(date)) %>% 
	mutate(days = interval(start_time, date)/dhours(1)) %>% 
	# filter(ID == "17C") %>% 
	split(.$id)


all_ibs2 <- all_ibs %>% 
	mutate(start_time = min(date)) %>% 
	mutate(days = interval(start_time, date)/dhours(1)) %>% 
	ungroup()

example_data <- read_csv(here("data-processed", "nitrate-abundances-processed.csv")) %>% 
	filter(population != "COMBO") %>% 
	filter(!is.na(log(RFU)), !is.na(days))


example_split <-  example_data %>% 
	# filter(grepl("C", well_plate)) %>% 
	split(.$well_plate)


### define Nathaniel's fitting function
nderiv<-function(fit, x, eps=1e-5){(predict(fit, x + eps) - predict(fit, x - eps))/(2 * eps)}

spline.slope <- function(df, n=201, eps=1e-5, span=2){
	x <- df$days
	y <- df$temperature
	time_point <- seq(min(x), max(x), length=n)
	growth <- nderiv(loess(y ~ x, degree=1, span=span), time_point)
	output <- top_n(filter(data.frame(growth, time_point), growth > 0), n = 1, wt = growth)
	return(output)
}


## fit each well
temperature_example <- C17 %>% 
	map_df(spline.slope, .id = "id")

all_ibs2 %>% 
	filter(id == "14B") %>% View

## join the growth rate results back to initial df
example_results <- left_join(all_ibs2, temperature_example)

example_results %>% 
	distinct(time_point)

example_results %>% 
	ggplot(aes(x = substrate, y = growth)) + geom_boxplot()


example_results %>% 
	ggplot(aes( x= days, y = temperature, group = id)) + geom_line() +
	geom_point(aes(x= time_point, y = temperature, color = substrate), data = example_results) 


dh35 <- all_ibs %>% 
	filter(temperature > 35) %>% 
	mutate(dh_35 = temperature - 35) %>%
	group_by(id) %>% 
	summarise_each(funs(sum), dh_35)

dh1 <- left_join(ws2, dh35, by = "id")

View(dh1)

dh1 %>% 
	group_by(substrate) %>%
	summarise_each(funs(mean, std.error), dh_35) %>% 
	ggplot(aes(x = substrate, y = mean)) + geom_point() +
	geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error))
	

dh1 %>% 
	mutate(dh_35 = ifelse(is.na(dh_35), 0, dh_35)) %>% 
	# filter(size < 64000000) %>% 
	ggplot(aes(x = log(size), y = dh_35/12, color = height)) + geom_point() +
	ylab("Degree hours above 35°C") + xlab("Rock size (cm^3)") + scale_color_viridis_c()
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


mod_thres <- lm(above20 ~ height*substrate, data = thres)
summary(mod_thres)

thres_long <- thres %>% 
	gather(key = threshold, value = time_points, contains("above"))


thres_long %>% 
	# filter(size < 64000000) %>% 
	filter(threshold == "above35") %>% 
	mutate(time = (time_points*5)/60) %>% 
	ggplot(aes(x = log(size), y = time)) + geom_point() +
	facet_wrap( ~ threshold, scales = "free") 


thres_long %>% 
	# filter(size < 1400000) %>% 
	mutate(time = (time_points*5)/60) %>% 
	ggplot(aes(x = log(size), y = time)) + geom_point() +
	geom_smooth(method = "lm", color = "black") +
	facet_wrap( ~ threshold, scales = "free") +
	ylab("Time spent above threshold (hours)") + xlab("Rock size (cm^3)")
ggsave("figures/whytecliffe_size_scale_thresholds.pdf", width = 9, height = 6)


thres_long %>% 
mutate(substrate = ifelse(substrate == "C", "Cobble", "Bench")) %>% 
	mutate(time = (time_points*5)/60) %>% 
	group_by(substrate, threshold) %>% 
	summarise_each(funs(mean, std.error), time) %>% 
	ggplot(aes(x = substrate, y = mean)) + geom_point() +
	geom_errorbar(aes(ymin = mean - std.error, ymax = mean + std.error), width = 0.2) +
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
	# filter(size < 64000000) %>% 
	ggplot(aes(x = log(size), y = temperature)) + geom_point() +
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
	ggplot(aes(x = log(size), y = dm_height, color = orientation)) + geom_point(size = 3) +
	geom_smooth(method = "lm", color = "black")
	