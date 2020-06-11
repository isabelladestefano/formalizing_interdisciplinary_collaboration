library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
require(cleanNLP)
require(udpipe)
require(stringi)
library(knitr)
library(GGally)
library(network)
library(sna)
library(tidytext)
library(textstem)
setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
df <- read_csv("vss_abstracts_all.csv")
df <- select(df, -X1)
glimpse(df)


(total <- nrow(df))

missingText <- df %>%
  filter(is.na(abstract)) %>%
  nrow()
missingText

missingText / total

df %>%
  group_by(year) %>%
  summarise(count = n())


byAuthor <- df %>%
  mutate(authors=str_replace_all(authors, ", Jr.", " Jr."),
         author=str_replace_all(authors, ",$", ""),
         author=strsplit(author, ", ")) %>%
  unnest(author) %>%
  mutate(authorAbbr = ifelse(word(author, -1) %in% c("II", "III", "IV", "Jr."), #catches
                             paste(substring(author,1,1), word(author, -2)),
                             paste(substring(author,1,1), word(author, -1))))
write_csv(byAuthor, "vss_byAuthor.csv")


byAuthor %>%
  filter(word(author,1) == "Dr.") %>%
  .$author
