library(tidyverse)
library(lme4)

# Very detailed article and primer on MRP http://www.princeton.edu/~jkastell/MRP_primer/mrp_primer.pdf
# Replication code at http://www.princeton.edu/~jkastell/mrp_primer.html

nz_2017 <- read_csv("nz_2017.csv")

model <- glmer(target_binary ~ Maori + Sex + Age + (1 + Maori | Area), data = nz_2017, family = binomial)

census <- read_csv("census.csv")

census$pred_cells <- predict(model,census,type = "response")

# MRP estimates by area
census %>%
  group_by(Area) %>%
  dplyr::summarise(value = weighted.mean(pred_cells,count))

# MRP estimates by area and Ethnic group
census %>% 
  group_by(Area,`Ethnic group`) %>%
  dplyr::summarise(value = weighted.mean(pred_cells,count))

# MRP disagreement between ethnic group estimates
census %>% 
  group_by(Area,`Ethnic group`) %>%
  dplyr::summarise(value = weighted.mean(pred_cells,count)) %>%
  spread(`Ethnic group`,value) %>%
  mutate(diff = Maori-`Non-Maori`) %>%
  gather(key,value,-Area) %>%
  ggplot(aes(x = reorder(Area,value), y = value, color = key)) +
  geom_point() + coord_flip()

# Mapping the support

to_merge <- census %>%
  group_by(Area) %>%
  dplyr::summarise(value = weighted.mean(pred_cells,count))

nz_sf <- read_rds("map.rds")

nz_sf <- left_join(nz_sf,to_merge %>% rename(value_for_color = value),c("TA2017_NAM"="Area"))

nz_sf <- nz_sf %>% filter(!(TA2017_NAM %in% c("Area Outside Territorial Authority", "Chatham Islands Territory")))

l <- c(range(to_merge$value)[1],weighted.mean(census$pred_cells,census$count),range(to_merge$value)[2])

l_100 <- seq(l[1],l[3],length.out =  100)

pos_center <- which.min(abs(l_100-l[2]))

pallette_mine <- c(colorRampPalette(c("blue", "white"))(pos_center-1),colorRampPalette(c("white", "yellow"))(100-pos_center)[-1])

ggplot(nz_sf) +
  geom_sf(aes(fill = value_for_color)) + 
  scale_fill_gradientn(name = "Percentage who said somewhat\nmore or much more. White color\nis national average.", colours = pallette_mine) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "How much should the government do to make amends for past\ninjustices committed against Māori?")

