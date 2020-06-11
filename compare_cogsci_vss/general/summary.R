setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis/compare_CogSci_VSS/general/")
library(tidyverse)
my_conf_colours = c(
  "CogSci" = "#027880",
  "VSS" = "#e0772f"
)
cogsci <- read_csv("cogsci_byAuthor.csv") %>%
vss <- read_csv("vss_byAuthor.csv")

full <- bind_rows(mutate(cogsci, conference="CogSci"), 
                  mutate(vss, conference="VSS"))

papersByYear <- full %>%
  dplyr::select(-c(author,authorAbbr)) %>%
  distinct() %>%
  group_by(conference, year) %>%
  summarise(papers=n())

authorsByYear <- full %>%
  dplyr::select(conference,year,authorAbbr) %>%
  distinct() %>%
  group_by(conference, year) %>%
  summarise(authors=n())

authorsPerPaperByYear <- full %>%
  dplyr::select(conference,year,title,authorAbbr) %>%
  distinct() %>%
  group_by(conference, year, title) %>%
  summarise(authorsPerPaper=n()) %>%
  ungroup() %>%
  group_by(conference, year) %>%
  summarise(meanAuthorsPerPaper=mean(authorsPerPaper))

fullSummary <- left_join(papersByYear, authorsByYear, by=c("conference","year"))
fullSummary <- left_join(fullSummary, authorsPerPaperByYear, by=c("conference","year"))
fullSummary

fullSummary %>%
  gather("measure","value",3:5) %>%
  ggplot(aes(x=year, y=value, colour=conference)) +
  geom_line() +
  scale_colour_manual(values=my_conf_colours) +
  facet_wrap(~measure, scales="free") +
  theme_minimal()
ggsave("img/growth.png")  
