

### CTmax data

ctmax <- read_csv("data-raw/Barnacle_Lab-CTmax.csv")

ctmax %>% 
	# filter(HabitatGroup == "Emergent_Rock") %>% 
	gather(key = rep, value = ctmax, 3:10) %>% 
	select(-Average_CTmax) %>% 
	filter(!is.na(ctmax)) %>% 
	group_by(Partners, HabitatGroup) %>% 
	summarise_each(funs(mean, std.error), ctmax) %>% 
	ggplot(aes(x = HabitatGroup, y = mean)) + geom_point() +
	geom_boxplot() + ylab("CTmax") + xlab("Habitat")



