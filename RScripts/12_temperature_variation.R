
library(tidyverse)
library(lubridate)

temps <- read_csv("data-processed/all_temps_w_tides.csv") %>% 
	mutate(region = ifelse(region == "Outercoast", "Outer Coast", region))
daytime_em <- temps %>% 
	filter(emersed == "emersed" & daytime == "daytime") %>% 
	mutate(region = ifelse(region == "Saltspring", "Gulf Islands", region)) %>% 
	mutate(region = factor(region, levels = c("Outer Coast", "Gulf Islands", "Vancouver")))

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

sd_temps

library(lme4)
library(nlme)
fit <- lmer(dh_35 ~ substrate + (1|site_rename), data= dh_38.5a)

m <- lme(sd_temp ~ substrate, random =  ~ 1| site_rename, sd_temps)

m1 <- lm(sd_temp ~ substrate, data = sd_temps)
summary(m1)

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




max_year <- daytime_em %>% 
	group_by(site_rename, substrate, region) %>%
	summarise_each(funs(mean, max), temperature)

quant_year <- daytime_em %>% 
	group_by(site_rename, substrate) %>% 
	summarise(ks_q2.5=quantile(temperature, probs=0.025),
						ks_q97.5=quantile(temperature, probs=0.975))

emersion9 <- left_join(emersion3b, sd_temps) %>% 
	left_join(., sd_temps_all)

emersion10 <- left_join(emersion3b, max_year) %>% 
	left_join(., quant_year)

plota <- emersion10 %>% 
	ggplot(aes(x = ks_q97.5, y = hours_emersed, color = substrate)) + 
	geom_smooth(method = "lm", color = "black") +
	geom_point(size = 4) +
	geom_point(size = 4, shape = 1, color = "black") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	ylab("Barnacle upper limit \n (Emersion time (hr/day))") +
	xlab("97.5 percentile of temperatures \n during summer") + 
	theme(legend.position = c(0.05, 0.18)) +
	ylim(3, 15) +
	theme(axis.text = element_text(size= 16),
				axis.title=element_text(size=16),
				legend.text = element_text(size= 16),
				legend.title = element_text(size= 16))

ggsave("figures/yearly-max-hours.png", width = 8, height = 6)
ggsave("figures/upper-quantile-hours.png", width = 6, height = 4)

lm(hours_emersed ~ ks_q97.5, data = emersion10) %>% tidy(conf.int = TRUE)

mod1 <- lmodel2(hours_emersed ~ ks_q97.5, data = emersion10)
mod1$regression.results
mod1$confidence.intervals
mod1$rsquare
cor(emersion9c$hours_emersed, emersion9c$sd_temp)

daytime_em %>% 
	ggplot(aes(x = temperature, fill = substrate)) + geom_histogram() +
	facet_wrap( ~ site_rename) +
	geom_vline(xintercept = 38.5) +
	xlim(38, 50)



emersion9b <- emersion9 %>% 
	ungroup() %>% 
	mutate(substrate = case_when(substrate == "cobble" ~"Cobble",
															 substrate == "bench" ~ "Bench")) %>% 
	ungroup() %>% 
	filter(!is.na(sd_temp_all))

unique(emersion9b$region)	

plotb <- emersion9b %>% 
ggplot(aes(x = sd_temp_all, y = hours_emersed, color = substrate)) +
	geom_smooth(method = "lm", color = "black") +
	geom_point(size = 4) +
	geom_point(size = 4, shape = 1, color = "black") + 
	# geom_smooth(method = "lm") +
	theme(strip.background = element_rect(colour="white", fill="white")) + 
	xlab("SD of temperatures during \n aerial exposure (°C)") +
	ylab("") +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	scale_color_viridis(discrete = TRUE, begin = 0.7, end = 0.9) +
	theme(legend.position = "none") + ylim(3, 15) +
	theme(axis.text = element_text(size= 16),
				axis.title=element_text(size=16))
	
ggsave("figures/upper-limit-temperature-SD-all-summer.png", width = 6, height = 4)


plots <-  plot_grid(plota, plotb, labels = c("A", "B"), align = "h", nrow = 1, ncol = 2)


save_plot("figures/figure2.png", plots,
					ncol = 2, # we're saving a grid plot of 2 columns
					nrow = 1, # and 2 rows
					# each individual subplot should have an aspect ratio of 1.3
					base_aspect_ratio = 1.1
)

write_csv(emersion10, "data-processed/emersion10.csv")

library(lmodel2)
?lmodel2
emersion9c <- emersion9b %>% 
	filter(substrate == "Bench")

lm(hours_emersed ~ sd_temp_all, data = emersion9b) %>% summary()

mod <- lmodel2(hours_emersed ~ sd_temp_all, data = emersion9b)
summary(mod)
mod$regression.results
mod$confidence.intervals
mod$rsquare
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
