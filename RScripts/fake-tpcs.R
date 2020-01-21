
### messing around 


library(tidyverse)
library(cowplot)

theme_set(theme_cowplot())

p <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
	# stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1, log = TRUE), size = 2, color = "black") +
	# stat_function(fun = dnorm, n = 50, args = list(mean = 0, sd = 1.2, log = TRUE), size = 2, color = "green") +
	stat_function(fun = dnorm, n = 101, args = list(mean = -0.5, sd = 1), size = 2, color = 'red') + ylab("") +
	# stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 2, color = 'purple') +ylab("") +
	stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 1), size = 2, color = 'orange') +
	ylab("") +
	scale_y_continuous(breaks = NULL) +
	scale_x_continuous(breaks = NULL) +
	xlab("") 

ggsave(p, filename = "figures/tpc1-multi.png",  bg = "transparent", width = 6, height = 4)


m <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
	# stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1, log = TRUE), size = 2, color = "black") +
	# stat_function(fun = dnorm, n = 50, args = list(mean = 0, sd = 1.2, log = TRUE), size = 2, color = "green") +
	# stat_function(fun = dnorm, n = 101, args = list(mean = -0.5, sd = 1), size = 2, color = 'red') + ylab("") +
	stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 2, color = 'purple') +
	stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1.5), size = 2, color = 'blue') +
	# stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 1), size = 2, color = 'orange') +
	ylab("") +
	scale_y_continuous(breaks = NULL) +
	scale_x_continuous(breaks = NULL) +
	xlab("") 

ggsave(m, filename = "figures/tpc1-gen-spec.png",  bg = "transparent", width = 6, height = 4)

 l <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
	# stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1, log = TRUE), size = 2, color = "black") +
	# stat_function(fun = dnorm, n = 50, args = list(mean = 0, sd = 1.2, log = TRUE), size = 2, color = "green") +
	# stat_function(fun = dnorm, n = 101, args = list(mean = -0.5, sd = 1), size = 2, color = 'red') + ylab("") +
	stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), size = 2, color = 'darkgrey') +
 	# stat_function(fun = dnorm, n = 101, args = list(mean = 0.7, sd = 1), size = 2, color = 'red') +
	# stat_function(fun = dnorm, n = 101, args = list(mean = -.1, sd = 1.7), size = 2, color = 'blue') +
	# stat_function(fun = dnorm, n = 101, args = list(mean = 0.5, sd = 1), size = 2, color = 'orange') +
	ylab("") +
	scale_y_continuous(breaks = NULL) +
	scale_x_continuous(breaks = NULL) +
	xlab("") 

ggsave(l, filename = "figures/met-melt-grey.png",  bg = "transparent", width = 3, height = 2)


ggsave("figures/tpc1-multi.png", width = 3, height = 2)

x <- seq(from = -3, to = 3, length = 101)
x
dnorm()
dnorm()