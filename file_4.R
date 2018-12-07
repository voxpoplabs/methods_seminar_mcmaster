library(tidyverse)
library(lme4)

# Very detailed article and primer on MRP http://www.princeton.edu/~jkastell/MRP_primer/mrp_primer.pdf
# Replication code at http://www.princeton.edu/~jkastell/mrp_primer.html

nz_2017 <- read_csv("nz_2017.csv")
nz_2017 <- read_csv("https://raw.githubusercontent.com/voxpoplabs/methods_seminar_mcmaster/master/nz_2017.csv")

model <- glmer(target_binary ~ Maori + Sex + Age + (1 + Maori | Area), data = nz_2017, family = binomial)

#Studying the random effects
ranef(model)

# More complex model with many interactions
#model <- glmer(target_binary ~ Maori * Sex * Age + (1 + Maori | Area), data = nz_2017, family = binomial)

#Run a full bayesian model using brms to obtain confidence intervals everywhere
# Might require install.packages("rcpp")
# With only 300 observations. Bayesian modelling can be long.
#library(brms)
#nz_2017_300  <- nz_2017[sample(1:nrow(nz_2017),300),]

#multi_logit <- brm(as.factor(target) ~ Maori + Sex + Age,
#                   data = nz_2017_300,
#                   family="categorical", chains=2, iter=400, warmup=200)

