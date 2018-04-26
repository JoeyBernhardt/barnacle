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
	ggplot(data = ., aes(x = Site, y = emersion_time_hours, fill = factor(substrate))) + geom_boxplot() + 
	ylab("hours of emersion time per day") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) + facet_wrap( ~ Site)


emersion_noBoulder %>% 
	ggplot(data = ., aes(x = substrate, y = emersion_time_hours, fill = factor(substrate))) + geom_bar(stat = "identity") + 
	ylab("hours of emersion time per day") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) + facet_wrap( ~ Site) +
	scale_fill_viridis(discrete = TRUE, begin = 0.7, end = 0.9)



mod1 <- lm(emersion_time_hours ~ substrate + Site, data = emersion_noBoulder)
summary(mod1)

levels(emersion_noBoulder$Region)
