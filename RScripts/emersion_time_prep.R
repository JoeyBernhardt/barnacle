### calculate emersion time

library(dplyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(tidyr)


barnacle <- read_csv("data-raw/survey_barnacle_sheep_updated.csv")
atkinson <- read_csv("tide_data/atkinson_2min.csv")
fulford <- read_csv("tide_data/fulford_1min.csv")

tides <- read_csv("data-processed/all_sites_1min_tides.csv") %>% 
	mutate(hour = hour(date)) %>% 
	mutate(day = day(date)) %>% 
	mutate(month = month(date)) %>% 
	mutate(year = 2012) %>% 
	unite(col = month_day_year, month, day, year, remove = FALSE) %>% 
	mutate(month_day_year = mdy(month_day_year))

tides_daytime <- tides %>%
	mutate(minute = 1) %>% 
	filter(hour > 6, hour < 20) 

unique(barnacle$Site)
unique(tides$site)

### get the barnacle data aligned with the tide data

barnacle2 <- barnacle %>% 
	mutate(tide_site = case_when(Site %in% c("RuckleGI", "SheepfarmGI1", "SheepfarmGI2",
																					 "SheepfarmGI3", "EaglecoveGI", "SookesGI", "SookesGI2",
																					 "EaglecoveGI", "EagleCoveGI", "SheeparmGI") ~ "fulford",
															 grepl("Welbury", Site) ~ "ganges",
				 											grepl("Ukie", Site) ~ "ucluelet",
															 grepl("Toquart", Site) ~ "stopper",
															 grepl("Tofino", Site) ~ "tofino",
															 grepl("Van", Site) ~ "atkinson",
															 grepl("Sheep", Site) ~ "fulford",
															 grepl("Sooke", Site) ~ "fulford",
															 grepl("Bamfield", Site) ~ "ucluelet")) %>% 
	mutate(obs_id = rownames(.)) %>% 
	clean_names()



df1 <- barnacle2 %>% 
	filter(obs_id == "101")

emersion_function <- function(df) {
	hours_emersed <- tides_daytime %>% 
		filter(site == df$tide_site) %>%
		filter(height < df$height_above_mllw[[1]]) %>%
		group_by(month_day_year) %>% 
		summarise_each(funs(sum), minute) %>% 
		summarise_each(funs(mean), minute) %>% 
		mutate(hours_emersed = minute/60)
	
	return(hours_emersed)
}	



barn_split <- barnacle2 %>% 
	split(.$obs_id)

barn_emersion <- barn_split %>% 
	map_df(emersion_function, .id = "obs_id") 


barnacle_emersion_times <- left_join(barn_emersion, barnacle2, by = "obs_id")

write_csv(barnacle_emersion_times, "data-processed/barnacle_emersion_times.csv") ## just note that I used Ukie tides for the bamfield site, come back and fix that!


# Gulf Islands data -------------------------------------------------------


View(gulf)

min(gulf$height.above.MLLW)
max(gulf$height.above.MLLW)

mean_height_gulf <- gulf %>% 
	# filter(Site != "SheepfarmGI") %>% 
	group_by(Site, substrate, Date) %>% 
	summarise(mean_height = mean(height.above.MLLW)) 

View(mean_height_gulf)


mean_height_gulf$site_number = rownames(mean_height_gulf)

## turn Time column into the right format
# fulford$Time <- hm(fulford$Time)


 ## take out incomplete days in May
fulford_trim <- fulford %>% 
	filter(Date != "12-07-25") %>% 
	filter(Date != "12-08-22") 

## create a new column with numbers for minutes
fulford_trim$minutes <-  seq(1,1440, 1)

60*24

fulford_trim$minute <- minute(fulford_trim$Time)

## remove all the time points that are not between 6am and 7pm (i.e. daytime hours)
View(fulford_trim)


fulford_trim <- fulford_trim %>% 
	filter(minutes > 361 & minutes < 1141)

## create new columns, one for each site
col.names <- paste0("S", 1:19)

## fill the columns with NAs
fulford_trim[, col.names] <- NA

fulford_trim[, col.names] <- rep(mean_height_gulf$mean_height, each = nrow(fulford_trim))

View(fulford_trim)

## now calculate the daytime minutes above barnacle level, for one site
may_fulford_1 <- fulford_trim %>% ## tide data
	filter(minutes > 361 & minutes < 1141) %>% 
	group_by(Date) %>% 
	filter(Tide.height < x) %>% 
	tally %>% 
	summarise(mean_minutes_above_barn = mean(n))
may_fulford_1$site <- "S1"

View(may_fulford_1)
## function to count the number of lines where the tide level is below to barnacle level
m  = NULL
emersion_time <- function(df, x) {
	for (i in seq_along(x)) {
		m[i] <- nrow(subset(df, Tide.height < x[i]))
		
	}
	return(m)
}


number_of_minutes <- emersion_time(fulford_trim, fulford_trim$Date, mean_height_gulf$mean_height)


d <- mean_height_gulf$mean_height

library(purrr)

by_day <- fulford_trim %>% 
	split(.$Date) %>%
	map_df( ~ emersion_time(., d)) %>% 
	as.data.frame 
	

View(by_day)
by_day$site_number <- rownames(by_day)

by_day_total <- by_day %>% 
	mutate(total = rowSums(.[1:29]))


# join with gulf island summary -------------------------------------------

by_day_total_gulf <- left_join(by_day_total, mean_height_gulf)

by_day_total_gulf %>% 
	group_by(substrate) %>% 
	summarise(mean_minutes_above = mean(total)) 

by_day_total_gulf %>% 
	ggplot(., aes(x = Site, y = total, group = as.factor(Date), color = substrate)) + geom_point(aes(shape = Date), size = 8) +
	ylab("daytime emersion time, minutes") +
	theme(axis.text=element_text(size=12),
				axis.title=element_text(size=14,face="bold")) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# Vancouver region data ---------------------------------------------------

vancouver <- barnacle %>% 
	filter(Region == "Vancouver")

atkinson <- read.csv("tide_data/atkinson_1min_july.csv")

mean_height_vancouver <- vancouver %>% 
	group_by(Site, substrate, Date) %>% 
	summarise(mean_height = mean(height.above.MLLW))


mean_height_vancouver$site_number = rownames(mean_height_vancouver)

h <- mean_height_vancouver$mean_height

## take out incomplete days in May
atkinson_trim <- atkinson %>% 
	filter(Date != "12-07-25") %>% 
	filter(Date != "12-08-22") 

## create a new column with numbers for minutes
atkinson_trim$minutes <-  seq(1,1440, 1)

## remove all the time points that are not between 6am and 7pm (i.e. daytime hours)

atkinson_trim <- atkinson_trim %>% 
	filter(minutes > 361 & minutes < 1141)

by_day_vancouver <- atkinson_trim %>% 
	split(.$Date) %>%
	map( ~ emersion_time(., h)) %>% 
	as.data.frame 

by_day_vancouver$site_number <- rownames(by_day_vancouver)

by_day_total_vancouver <- by_day_vancouver %>% 
	mutate(total = rowSums(.[1:29]))


# join with Vancouver summary data ----------------------------------------

by_day_vancouver <- left_join(by_day_total_vancouver, mean_height_vancouver)

by_day_vancouver %>% 
	ggplot(., aes(x = Site, y = total, group = as.factor(Date), color = substrate)) + geom_point(aes(shape = Date), size = 8) +
	ylab("daytime emersion time, minutes") +
	theme(axis.text=element_text(size=12),
				axis.title=element_text(size=14,face="bold")) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave("vancouver_emersion_time.png")



# Outer Coast region ------------------------------------------------------

outer_coast <- barnacle %>% 
	filter(Region == "outer coast")

Tofino <- outer_coast %>% 
	filter(., grepl("Tofino", Site)) 

Bamfield <- outer_coast %>% 
	filter(., grepl("Bamfield", Site)) 

Ukie <- outer_coast %>% 
	filter(., grepl("Ukie", Site)) 

Toquart <- outer_coast %>% 
	filter(., grepl("Toquart", Site)) 



# Tofino ------------------------------------------------------------------

tofino <- read.csv("tide_data/Tofino_July2012_1min.csv")

mean_height_tofino <- Tofino %>% 
	group_by(Site, substrate, Date) %>% 
	summarise(mean_height = mean(height.above.MLLW))


mean_height_tofino$site_number = rownames(mean_height_tofino)

t <- mean_height_tofino$mean_height

## take out incomplete days in May
tofino_trim <- tofino %>% 
	filter(Date != "12-07-25") %>% 
	filter(Date != "12-08-22") 

## create a new column with numbers for minutes
tofino_trim$minutes <-  seq(1,1440, 1)

## remove all the time points that are not between 6am and 7pm (i.e. daytime hours)

tofino_trim <- tofino_trim %>% 
	filter(minutes > 361 & minutes < 1141)

by_day_tofino <- tofino_trim %>% 
	split(.$Date) %>%
	map( ~ emersion_time(., t)) %>% 
	as.data.frame 

by_day_tofino$site_number <- rownames(by_day_tofino)

by_day_total_tofino <- by_day_tofino %>% 
	mutate(total = rowSums(.[1:29]))

by_day_tofino <- left_join(by_day_total_tofino, mean_height_tofino)



# Toquart -----------------------------------------------------------------

toquart <- read.csv("tide_data/stopper_islands_1min_July2012.csv")

mean_height_toquart <- Toquart %>% 
	group_by(Site, substrate, Date) %>% 
	summarise(mean_height = mean(height.above.MLLW))


mean_height_toquart$site_number = rownames(mean_height_toquart)

tq <- mean_height_toquart$mean_height

## take out incomplete days in May
toquart_trim <- toquart %>% 
	filter(Date != "12-07-25") %>% 
	filter(Date != "12-08-22") 

## create a new column with numbers for minutes
toquart_trim$minutes <-  seq(1,1440, 1)

## remove all the time points that are not between 6am and 7pm (i.e. daytime hours)

toquart_trim <- toquart_trim %>% 
	filter(minutes > 361 & minutes < 1141)

by_day_toquart <- toquart_trim %>% 
	split(.$Date) %>%
	map( ~ emersion_time(., tq)) %>% 
	as.data.frame 

by_day_toquart$site_number <- rownames(by_day_toquart)

by_day_total_toquart <- by_day_toquart %>% 
	mutate(total = rowSums(.[1:29]))

by_day_toquart <- left_join(by_day_total_toquart, mean_height_toquart)


# Ukie --------------------------------------------------------------------

ukie <- read.csv("tide_data/Ukie_July2012_1min.csv")

mean_height_ukie <- Ukie %>% 
	group_by(Site, substrate, Date) %>% 
	summarise(mean_height = mean(height.above.MLLW))


mean_height_ukie$site_number = rownames(mean_height_ukie)

u <- mean_height_ukie$mean_height

## take out incomplete days in May
ukie_trim <- ukie %>% 
	filter(Date != "12-07-25") %>% 
	filter(Date != "12-08-22") 

## create a new column with numbers for minutes
ukie_trim$minutes <-  seq(1,1440, 1)

## remove all the time points that are not between 6am and 7pm (i.e. daytime hours)

ukie_trim <- ukie_trim %>% 
	filter(minutes > 361 & minutes < 1141)

by_day_ukie <- ukie_trim %>% 
	split(.$Date) %>%
	map( ~ emersion_time(., u)) %>% 
	as.data.frame 

by_day_ukie$site_number <- rownames(by_day_ukie)

by_day_total_ukie <- by_day_ukie %>% 
	mutate(total = rowSums(.[1:29]))

by_day_ukie <- left_join(by_day_total_ukie, mean_height_ukie)


# Bamfield ----------------------------------------------------------------

bamfield <- read.csv("tide_data/Bamfield_1min_july2012.csv")

mean_height_bamfield <- Bamfield %>% 
	group_by(Site, substrate, Date) %>% 
	summarise(mean_height = mean(height.above.MLLW))


mean_height_bamfield$site_number = rownames(mean_height_bamfield)

b <- mean_height_bamfield$mean_height

## take out incomplete days in May
bamfield_trim <- bamfield %>% 
	filter(Date != "12-07-25") %>% 
	filter(Date != "12-08-22") 

## create a new column with numbers for minutes
bamfield_trim$minutes <-  seq(1,1440, 1)

## remove all the time points that are not between 6am and 7pm (i.e. daytime hours)

bamfield_trim <- bamfield_trim %>% 
	filter(minutes > 361 & minutes < 1141)

by_day_bamfield <- bamfield_trim %>% 
	split(.$Date) %>%
	map( ~ emersion_time(., b)) %>% 
	as.data.frame 

by_day_bamfield$site_number <- rownames(by_day_bamfield)

by_day_total_bamfield <- by_day_bamfield %>% 
	mutate(total = rowSums(.[1:29]))

by_day_bamfield <- left_join(by_day_total_bamfield, mean_height_bamfield)


# merging all of the emersion times ---------------------------------------

all <- bind_rows(by_day_bamfield, by_day_ukie, by_day_toquart, by_day_tofino, by_day_total_gulf, by_day_vancouver)

all_emersion <- all %>% 
	select(31:35)

all_emersion %>% 
	ggplot(data = ., aes(x = mean_height, y = total, group = substrate, color = Site)) + geom_point(aes(shape = substrate), size = 4)

write.csv(all_emersion, "emersion_time_upperlimits.csv")


# plots -------------------------------------------------------------------

emersion <- read.csv("data-processed/emersion_time_upperlimits.csv")

str(emersion)

emersion$Region <- ordered(emersion$Region, levels = c("gulf_islands", "vancouver", "outer_coast"))

emersion %>% 
	ggplot(data = ., aes(x = mean_height, y = total, group = Region, color = substrate)) + geom_point(aes(shape = Region), size = 4)


emersion <- emersion %>% 
	mutate(emersion_hours = ((total/60)/29))

emersion %>% 
	filter(., !grepl("Ukie", Site)) %>% 
	filter(substrate != "boulder") %>% 
	ggplot(data = ., aes(x = Region, y = emersion_hours, fill = factor(substrate))) + geom_boxplot() + 
	ylab("daytime emersion time, hours") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) 
ggsave("allsites_emersion.png")
	
	
# Extra code section ------------------------------------------------------



## plot it!
str(emersion_gulf_sites)
emersion_gulf_sites %>% 
	ggplot(., aes(x = Site, y = mean_minutes_above_barn, group = as.factor(Date), color = substrate)) + geom_point(aes(shape = Date), size = 8) +
	ylab("daytime emersion time, minutes") +
	theme(axis.text=element_text(size=12),
				axis.title=element_text(size=14,face="bold")) +
	theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
ggsave("gulf_islands_emersion.png")
