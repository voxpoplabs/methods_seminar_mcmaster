library(tidyverse)
library(lme4)

# Very detailed article and primer on MRP http://www.princeton.edu/~jkastell/MRP_primer/mrp_primer.pdf
# Replication code at http://www.princeton.edu/~jkastell/mrp_primer.html

nz_2017 <- read_csv("nz_2017.csv")
nz_2017 <- read_csv("https://raw.githubusercontent.com/voxpoplabs/methods_seminar_mcmaster/master/nz_2017.csv")

model <- glmer(target_binary ~ Maori + Sex + Age + (1 + Maori | Area), data = nz_2017, family = binomial)

census <- read_csv("https://raw.githubusercontent.com/voxpoplabs/methods_seminar_mcmaster/master/census.csv")
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

