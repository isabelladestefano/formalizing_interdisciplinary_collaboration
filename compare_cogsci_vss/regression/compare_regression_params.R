setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis/compare_CogSci_VSS/regression")
library(tidyverse)
my_conf_colours = c(
  "CogSci" = "#027880",
  "VSS" = "#e0772f"
)
# cogsci_regression <- read_csv("cogsci_regressionParam.csv") %>%
#   mutate(conference="CogSci",
#          parameters=c("Intercept","Prior_Publication","NegativeLogTopicSim"))
# vss_regression <- read_csv("vss_regressionParam.csv") %>%
#   mutate(conference="VSS",
#          parameters=c("Intercept","Prior_Publication","NegativeLogTopicSim"))
# 
# all_regression <- bind_rows(cogsci_regression, vss_regression)

cogsci_regression <- data.frame(
  parameters=c("Intercept","prior_publication","logCosTopicSim","Interaction"),
  estimate=c(-6.75589,4.59234,2.24802,-3.86154),
  std.err=c(0.04371,0.04861,0.05323,0.15808)
)

vss_regression <- data.frame(
  parameters=c("Intercept","prior_publication","logCosTopicSim","Interaction"),
  estimate=c(-6.63778,5.21826,2.59832,-3.96041),
  std.err=c( 0.02545,0.02817,0.03526,0.08264)
)

all_regression <- bind_rows(cogsci_regression, vss_regression)

# all_regression %>%
#   filter(parameters == "logCosTopicSim") %>%
#   mutate(lower=estimate-Std..Error,
#          upper=Estimate+Std..Error) %>%
#   ggplot(aes(x=conference, y=Estimate, fill=conference)) +
#   geom_pointrange(aes(ymin=lower, ymax=upper)) +
#   #geom_errorbar(aes(ymin=lower, ymax=upper), width=0.5) +
#   scale_y_continuous(limits=c(1,2)) +
#   scale_fill_manual(values=my_conf_colours) +
#   theme_minimal() +
#   theme(legend.position="none")

all_regression

# one sided 2 sample t test with pooled variance
wald.z.test <- function(m1, sd1, m2, sd2){
  z <- (m1 - m2) / sqrt(sd1^2 + sd2^2)
  p <- pnorm(abs(z), lower.tail=F)
  return(data.frame(m1, sd1, m2, sd2, z, p))
}

testParam <- filter(all_regression, parameters=="logCosTopicSim")
wald.z.test(testParam$estimate[1], testParam$std.err[1], testParam$estimate[2], testParam$std.err[2])

interParam <- filter(all_regression, parameters=="Interaction")
wald.z.test(interParam$estimate[1], interParam$std.err[1], interParam$estimate[2], interParam$std.err[2])


