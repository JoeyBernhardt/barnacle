## Analysis of upper limits

library(dplyr)
library(ggplot2)

emersion <- read.csv("emersion_time_upperlimits.csv")

emersion_noBoulder <- emersion %>% 
	filter(substrate != "boulder")

emersion.fit <- lm(total ~ substrate + Region, data = emersion_noBoulder)
summary(emersion.fit)

height.fit <- lm(mean_height ~ substrate + Region, data = emersion_noBoulder)
summary(height.fit)


hist(emersion_noBoulder$total)

ggplot(data = emersion_noBoulder, aes(x = ))

emersion_noBoulder %>% 
ggplot(data = ., aes(x = Region, y = mean_height, fill = factor(substrate))) + geom_boxplot() + 
	ylab("daytime emersion time, hours") +
	theme(axis.text=element_text(size=16),
				axis.title=element_text(size=14,face="bold")) 
