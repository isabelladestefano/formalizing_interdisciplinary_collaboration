setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(lme4)
cogsci_byAuthor = read_csv("cogsci_byAuthor.csv")

central = read_csv("networkByYear/centrality_2019.csv")

# model_data <- data.frame()
# for(i in 2000:2019){
#   subset_data <- read_csv(paste0("topicCoauthMatr/topicCoauth",i,".csv")) %>%
#     mutate(topicSim = authorsSim) %>%
#     dplyr::select(-c(X1, authorsSim))
#   model_data <- bind_rows(model_data, subset_data)
# }
# write_csv(model_data, "full_model_data.csv")
model_data <- read_csv("full_model_data.csv")




glimpse(model_data)
model_data %>%
  filter((authorA == "E Vul" | authorB == "E Vul") & prior_publication == 1)

nrow(model_data)
model_data %>%
  filter(is.na(prior_publication)) %>%
  group_by(authorA) %>%
  summarise(n=n()) %>%
  dplyr::arrange(desc(n)) # some authors missing from coauthorship matrix


data.pre2019 <- model_data %>%
  filter(year < 2019 & !is.na(prior_publication)) %>%
  mutate(neglogTopicSim = log10(pi-topicSim),
         cosTopicSim = cos(topicSim),
         logCosTopicSim = log10(cosTopicSim))

m.logcos.interact <- glm(new_publication ~ prior_publication + logCosTopicSim + prior_publication:logCosTopicSim, data=data.pre2019, family=binomial())
summary(m.logcos.interact)

write_csv(data.frame(summary(m.logcos.interact)$coeff), "cogsci_regressionParam.csv")


## predict 2019
pred2019.df <- model_data %>%
  filter(year == 2019) %>%
  mutate(cosTopicSim = cos(topicSim),
         logCosTopicSim = log10(cosTopicSim))
predict.m <- predict.glm(m.logcos.interact, newdata=dplyr::select(pred2019.df,c(prior_publication,logCosTopicSim)), family="binomial")
logOdds.to.Prob <- function(y){
  exp(y)/(1+exp(y))
}
pred2019.df.interact <-pred2019.df
pred2019.df.interact$predicted_new <- logOdds.to.Prob(predict.m)
# (quants <- quantile(pred2019.df$predicted_new, 0.5, na.rm=TRUE))
# pred2019.df <- pred2019.df %>%
#   mutate(predicted_new.binary = ifelse(predicted_new >= quants, TRUE, FALSE))
write.csv(pred2019.df.interact, "model_prediction_logcos.csv")





# subset of 2019 authors
multAuthors <- cogsci_byAuthor %>%
  filter(year >= 2015) %>%
  group_by(authorAbbr) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  .$authorAbbr
head(central_n)
central_n <- central %>%
  mutate(CM = as.factor(CM)) %>%
  filter(CM=="eigen" & label %in% multAuthors) %>%
  dplyr::arrange(desc(measure)) %>%
  top_n(50, measure) %>%
  dplyr::arrange(label) %>%
  mutate(index=1:nrow(.))
us <- central %>%
  mutate(CM = as.factor(CM)) %>%
  filter(CM=="eigen" & label %in% c("E Vul")) %>%
  dplyr::arrange(desc(measure), label) %>%
  mutate(index=1:nrow(.))
central_n <- bind_rows(central_n, us)

centralA <- central_n %>%
  mutate(authorA = label,
         indexA = index) %>%
  dplyr::select(c(authorA, indexA))
centralB <- central_n %>%
  mutate(authorB = label,
         indexB = index) %>%
  dplyr::select(c(authorB, indexB))
head(central)

# 2020 prediction from model
pred2019.df %>%
  filter(authorA %in% centralA$authorA & authorB %in% centralB$authorB) %>%
  left_join(centralA, by=c("authorA")) %>%
  left_join(centralB, by=c("authorB")) %>%
  #mutate(likely = ifelse(new_publication > 1/2003, "likely","unlikely")) %>%
  ggplot(aes(x=reorder(authorB, desc(authorB)), y=authorA, fill=predicted_new)) +
  geom_raster() +
  ggtitle("Predicted 2020 Co-authorships") +
  scale_x_discrete("") +
  scale_y_discrete("") +
  scale_fill_continuous("Probability") +
  theme(text = element_text(size=7),
        axis.text.x = element_text(angle=45, hjust=1))

ggsave("img/predicted2020_reduced.png", height=7, width=7.2)




# additive model predictions

m.logcos.add <- glm(new_publication ~ prior_publication + logCosTopicSim, data=data.pre2019, family=binomial())
summary(m.logcos.add)
predict.m.add <- predict.glm(m.logcos.add, newdata=dplyr::select(pred2019.df,c(prior_publication,logCosTopicSim)), family="binomial")

pred2019.df.add <- pred2019.df
pred2019.df.add$predicted_new <- logOdds.to.Prob(predict.m.add)
write.csv(pred2019.df.add, "model_prediction_logcos_additive.csv")



# only prior publication predictions


m.logcos.priorPub <- glm(new_publication ~ prior_publication, data=data.pre2019, family=binomial())
summary(m.logcos.priorPub)
predict.m.priorPub <- predict.glm(m.logcos.priorPub, newdata=dplyr::select(pred2019.df,c(prior_publication)), family="binomial")

pred2019.df.priorPub <- pred2019.df
pred2019.df.priorPub$predicted_new <- logOdds.to.Prob(predict.m.priorPub)
write.csv(pred2019.df.priorPub, "model_prediction_logcos_priorPub.csv")



anova(m.logcos.add,m.logcos.interact)



anova(m.logcos.interact)








# model we'll be using?
# m.log <- glm(new_publication ~ prior_publication + neglogTopicSim, data=data.pre2019, family=binomial())
# summary(m.log)
# 
# m.log.interact <- glm(new_publication ~ prior_publication * neglogTopicSim, data=data.pre2019, family=binomial())
# summary(m.log.interact)

# m.line <- glm(new_publication ~ prior_publication + topicSim, data=data.pre2019, family=binomial())
# summary(m.line)
# 
# m.base <- glm(new_publication ~ prior_publication, data=data.pre2019, family=binomial())
# summary(m.base)

# anova(m.base, m.logcos.interact, test='Chisq')



# m.log.2000 <- glm(new_publication ~ prior_publication + neglogTopicSim, data=filter(data.pre2019, year==2000), family=binomial())
# summary(m.log.2000)
# 
# m.log.2018 <- glm(new_publication ~ prior_publication + neglogTopicSim, data=filter(data.pre2019, year==2018), family=binomial())
# summary(m.log.2018)

# m.base <- glm(new_publication ~ prior_publication, data=df, family=binomial())
# summary(m.base)
# 
# anova(m.quad, m.base,test='Chisq')








