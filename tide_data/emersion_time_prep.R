### calculate emersion time
install.packages("tidyr")
library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(tidyr)




barnacle <- read.csv("survey_barnacle.csv")
atkinson <- read.csv("atkinson_2min.csv")
fulford <- read.csv("fulford_1min.csv")


vancouver <- barnacle %>% 
	filter(Region == "Vancouver")


gulf <- barnacle %>% 
	filter(Region == "gulfislands")

mean_height_vancouver <- vancouver %>% 
	group_by(Site, substrate) %>% 
	summarise(mean_height = mean(height.above.MLLW))


mean_height_gulf <- gulf %>% 
	filter(Site != "SheepfarmGI") %>% 
	group_by(Site, substrate, Date) %>% 
	summarise(mean_height = mean(height.above.MLLW)) 


mean_height_gulf$site_number = rownames(mean_height_gulf)

## turn Time column into the right format
fulford$Time <- hm(fulford$Time)


 ## take out incomplete days in May
fulford_trim <- fulford %>% 
	filter(Date != "12-07-25") %>% 
	filter(Date != "12-08-22") 

## create a new column with numbers for minutes
fulford_trim$minutes <-  seq(1,1440, 1)
fulford_trim$S1 <-  rep(1.6954150)
fulford_trim$S2 <-  rep(0.5662483)
fulford_trim$S3 <-  rep(2.9110116)
fulford_trim$S4 <-  rep(1.7207259)
fulford_trim$S5 <-  rep(2.8759237)
fulford_trim$S6 <-  rep(2.4987813)
fulford_trim$S7 <-  rep(2.7710828)
fulford_trim$S8 <-  rep(1.2672109)
fulford_trim$S9 <-  rep(0.4907108)
fulford_trim$S10 <-  rep(1.9806030)
fulford_trim$S11 <-  rep(0.6922696)
fulford_trim$S12 <-  rep(1.4503138)
fulford_trim$S13 <-  rep(2.8965473)
fulford_trim$S14 <-  rep(2.6225591)
fulford_trim$S15 <-  rep(2.7602922)
fulford_trim$S16 <-  rep(1.8135591)
fulford_trim$S17 <-  rep(2.3489827)

## now calculate the daytime minutes above barnacle level
may_fulford_1 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>% 
	group_by(Date) %>% 
	filter(Tide.height < S1) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_1$site <- "S1"


### try to make this into a function

may_fulford_2 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S2) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_2$site <- "S2"


may_fulford_3 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S3) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_3$site <- "S3"


may_fulford_4 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S4) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_4$site <- "S4"

may_fulford_5 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S5) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n))
may_fulford_5$site <- "S5"


may_fulford_6 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S6) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_6$site <- "S6"


may_fulford_7 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S7) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n))
may_fulford_7$site <- "S7"

may_fulford_8 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S8) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_8$site <- "S8"

may_fulford_9 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S9) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n))
may_fulford_9$site <- "S9"

may_fulford_10 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S10) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n))
may_fulford_10$site <- "S10"

may_fulford_11 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S11) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_11$site <- "S11"

may_fulford_12 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S12) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n))
may_fulford_12$site <- "S12"

may_fulford_13 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S13) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n))
may_fulford_13$site <- "S13"

may_fulford_14 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S14) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_14$site <- "S14"

may_fulford_15 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S15) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_15$site <- "S15"

may_fulford_16 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S16) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n))
may_fulford_16$site <- "S16"

may_fulford_17 <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141) %>%  
	group_by(Date) %>% 
	filter(Tide.height < S17) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n)) 
may_fulford_17$site <- "S17"

#### now bind all the minute dfs together


all_emersion_gulf <- bind_rows(may_fulford_1,
															 may_fulford_2,
															 may_fulford_3,
															 may_fulford_4,
															 may_fulford_5,
															 may_fulford_6,
															 may_fulford_7,
															 may_fulford_8,
															 may_fulford_9,
															 may_fulford_10,
															 may_fulford_11,
															 may_fulford_12,
															 may_fulford_13,
															 may_fulford_14,
															 may_fulford_15,
															 may_fulford_16,
															 may_fulford_17)


all_emersion_gulf <- separate(all_emersion_gulf, site, into = c("S", "site_number"), sep = 1) %>% 
	select(-S)


emersion_gulf_sites <- bind_cols(mean_height_gulf, all_emersion_gulf) %>% 
	as.data.frame()


## plot it!
str(emersion_gulf_sites)
emersion_gulf_sites %>% 
	ggplot(., aes(x = Site, y = mean_minutes_above_barn, group = as.factor(Date), color = substrate)) + geom_point(aes(shape = Date), size = 8) +
	ylab("daytime emersion time, minutes") +
	theme(axis.text=element_text(size=12),
				axis.title=element_text(size=14,face="bold")) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave("gulf_islands_emersion.png")
