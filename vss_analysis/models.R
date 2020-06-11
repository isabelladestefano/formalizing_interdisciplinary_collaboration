setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
library(tidyverse)
library(lme4)
vss_byAuthor = read_csv("vss_byAuthor.csv")

# model_data <- data.frame()
# for(i in 2001:2019){
#   subset_data <- read_csv(paste0("topicCoauthMatr/topicCoauth",i,".csv")) %>%
#     dplyr::select(-X1)
#   model_data <- bind_rows(model_data, subset_data)
# }
# write.csv(model_data, "vss_full_model_data.csv")
model_data <- read_csv("vss_full_model_data.csv") %>%
  select(-X1)

glimpse(model_data)
model_data %>%
  filter((authorA == "E Vul" | authorB == "E Vul") & prior_publication == 1)

nrow(model_data)
model_data %>%
  filter(is.na(prior_publication)) %>%
  group_by(authorA) %>%
  summarise(n=n()) %>%
  dplyr::arrange(desc(n)) # some authors missing from coauthorship matrix



# model with 1 previous year
df <- model_data %>%
  filter(year < 2019 & !is.na(prior_publication)) %>%
  mutate(neglogTopicSim = log10(pi-topicSim),
         cosTopicSim = cos(topicSim),
         logCosTopicSim = log10(cosTopicSim))
glimpse(df)

df %>%
  ggplot(aes(x=logCosTopicSim)) +
  geom_density()

# vss.base <- glm(new_publication ~ prior_publication, data=df, family=binomial())
# summary(vss.base)
# 
# vss.additive <- glm(new_publication ~ prior_publication + logCosTopicSim, data=df, family=binomial())
# summary(vss.additive)

m.logcos.interact <- glm(new_publication ~ prior_publication * logCosTopicSim, data=df, family=binomial())
summary(m.logcos.interact)
vss.logcos.interact <- m.logcos.interact

anova(vss.base, vss.additive, test='Chisq')
anova(vss.additive, vss.logcos.interact, test='Chisq')


m.log.2001 <- glm(new_publication ~ prior_publication + neglogTopicSim, data=filter(df, year==2001), family=binomial())
summary(m.log.2001)

m.log.2018 <- glm(new_publication ~ prior_publication + neglogTopicSim, data=filter(df, year==2018), family=binomial())
summary(m.log.2018)

# m.base <- glm(new_publication ~ prior_publication, data=df, family=binomial())
# summary(m.base)
# 
# anova(m.base, m.log,test='Chisq')

## predict 2019
pred2019.df <- model_data %>%
  filter(year == 2019) %>%
  mutate(neglogTopicSim = log(pi/2-topicSim))
predict.m <- predict.glm(m.log, newdata=dplyr::select(pred2019.df,c(prior_publication,neglogTopicSim)), family="binomial")

logOdds.to.Prob <- function(y){
  exp(y)/(1+exp(y))
}
pred2019.df$new_publication <- logOdds.to.Prob(predict.m)

write.csv(pred2019.df, "model_prediction.csv")




#### Model data with 5 years ####################


model_data.5 <- data.frame()
for(i in 2005:2019){
  subset_data <- read_csv(paste0("topicCoauthMatr.5/topicCoauth",i,".5.csv"), 
                          col_types = cols(prior_publication_minus1=col_number(),
                                           prior_publication_minus2=col_number(),
                                           prior_publication_minus3=col_number(),
                                           prior_publication_minus4=col_number(),
                                           prior_publication_minus5=col_number())) %>%
    mutate(year=i,
           prior_publication_minus2=ifelse(is.na(prior_publication_minus2), 0, prior_publication_minus2),
           prior_publication_minus3=ifelse(is.na(prior_publication_minus3), 0, prior_publication_minus3),
           prior_publication_minus4=ifelse(is.na(prior_publication_minus4), 0, prior_publication_minus4),
           prior_publication_minus5=ifelse(is.na(prior_publication_minus5), 0, prior_publication_minus5)) %>%
    dplyr::select(-c(X1))
  model_data.5 <- bind_rows(model_data.5, subset_data)
}
write_csv(model_data.5, "full_model_data.5.csv")

model_data.5 %>%
  filter((authorA == "N Goodman" | authorB == "N Goodman") & prior_publication_minus1 == 1)

data.pre2019.5 <- model_data.5 %>%
  filter(year < 2019 & !is.na(topicSim) & 
           !is.na(prior_publication_minus1) & !is.na(prior_publication_minus2) &
           !is.na(prior_publication_minus3) & !is.na(prior_publication_minus4) &
           !is.na(prior_publication_minus5))
head(data.pre2019.5)

round(cor(dplyr::select(data.pre2019.5, c(prior_publication_minus1, prior_publication_minus2, 
                                          prior_publication_minus3, prior_publication_minus4, 
                                          prior_publication_minus5, topicSim, new_publication))),4)

m5.quad <- glm(new_publication ~ prior_publication_minus1 + prior_publication_minus2 +
                 prior_publication_minus3 + prior_publication_minus4 +
                 prior_publication_minus5 + poly(topicSim,2), data=data.pre2019.5, family=binomial())
summary(m5.quad)
