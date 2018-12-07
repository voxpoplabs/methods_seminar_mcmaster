library(tidyverse)
library(lme4)

# Loading the data
# read_csv is a common alternative to read.csv which reads data and
# coerces to a tibble instead of a data frame.
# very similar to data frame, see 
# https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html

nz_2017 <- read_csv("nz_2017.csv")
nz_2017 <- read_csv("https://raw.githubusercontent.com/voxpoplabs/methods_seminar_mcmaster/master/nz_2017.csv")

# OLS regression. 
# target is How much should the government do to make amends for past 
# injustices committed against Māori? (Likert-scale 5)

model_lm <- lm(target ~ Maori + Sex + Age, data = nz_2017)

summary(model_lm)

# Maori are 0.79 higher on a five point scale

# One could use a multinomial logistic regression using nnet
# install.packages("nnet")
# library(nnet)
# multi_logit <- multinom(as.factor(target) ~ Maori + Sex + Age, data = nz_2017)

# Output predicted probabilities for each different unique possibility

# to_predict <- unique(nz_2017[,c("Age", "Maori","Sex")])
# probabilities <- predict(multi_logit,to_predict,type = "prob")
# bind_cols(to_predict,as_data_frame(probabilities))

multi_logit <- multinom(as.factor(target) ~ Maori * Sex + Maori * Age, data = nz_2017)

# Output predicted probabilities for each different unique possibility

# to_predict <- unique(nz_2017[,c("Age", "Maori","Sex")])
# probabilities <- predict(multi_logit,to_predict,type = "prob")
# bind_cols(to_predict,as_data_frame(probabilities))

#bind_cols(to_predict,as_data_frame(probabilities)) %>%
#  gather(answer_choice,predicted_probability,-Age,-Maori,-Sex) %>%
#  ggplot(aes(x=as.factor(answer_choice),y=predicted_probability,fill = Maori)) +
#  geom_bar(position = "dodge", stat = "identity") +
#  facet_grid(rows = vars(Age), cols = vars(Sex)) +
#  labs(x = "", y = "", title = "How much should the government do to make amends for past injustices committed against Māori?") +
#  scale_y_continuous(labels = function(x){paste0(x*100," %")}, limits = c(0,max(probabilities)*1.15)) + 
#  scale_x_discrete(labels = c("Much\nless","Somewhat\nless","About the same\nas now",
#                              "Somewhat\nmore","Much\nmore")) +
#  geom_text(aes(label=paste0(round(predicted_probability*100),"")),
#            color="black",
#            position=position_dodge(width=0.85),
#            vjust=-0.3)

