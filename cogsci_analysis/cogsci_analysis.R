# This script takes the raw paper data and parses it into a file called "cogsci_byAuthor.csv" which is used for network analysis 

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
knitr::opts_chunk$set(echo = TRUE)
df <- read_csv("cogsci_papers.csv", col_types = cols(abstract=col_character())) #this data set can be downloaded on osf (see README)

glimpse(df)

(total <- nrow(df))

df %>%
  filter(year >= 2000) %>%
  nrow()

missingText <- df %>%
  filter(is.na(full_text)) %>%
  nrow()
missingText

missingText / total

df %>%
  group_by(year) %>%
  summarise(count = n())


df$length <- str_count(df$full_text, pattern=" ")+1


editAuthors <- function(origAuthors){
  temp <- matrix(strsplit(origAuthors, ",")[[1]], , 2, byrow=TRUE)
  author <- paste(trimws(temp[,2]), trimws(temp[,1]), sep=" ")
  return(author)
}

byAuthor1 <- df%>%
  filter(year <= 2014 & year >= 2000) %>%
  mutate(author = mapply(editAuthors, authors)) %>%
  unnest(author) %>%
  mutate(authorAbbr = case_when(
    word(author, -1) %in% c("II", "III", "IV", "Jr.", "Jr", "Ph.D.", "PhD") ~ paste(substring(author,1,1), word(author, -2)),
    word(author, 1) %in% c("Dr.", "Dr") ~ paste(substring(word(author,2),1,1), word(author, -1)),
    TRUE ~ paste(substring(author,1,1), word(author, -1))
  ))


byAuthor2 <- df %>%
  filter(year > 2014 | year < 2000) %>%
  mutate(authors=stri_trans_general(authors, "latin-ascii"),
         author=str_replace_all(authors, ",$", ""),
         author=str_replace_all(author, c(".*\n"="", " *\\(.*?\\)"="", "  "=" ")),
         author=str_replace_all(author, " & ", ", "),
         author=strsplit(author, ", ")) %>%
  unnest(author) %>%
  filter(!grepl("University|Institute|Center|Centre|Centro|School|Department|Dept|Unit|Hospital|\\d",
                author)) %>%
  mutate(author=trimws(author),
         authorAbbr = case_when(
           word(author, -1) %in% c("II", "III", "IV", "Jr.", "Jr", "Ph.D.", "PhD") ~ paste(substring(author,1,1), word(author, -2)),
           word(author, 1) %in% c("Dr.", "Dr") ~ paste(substring(word(author,2),1,1), word(author, -1)),
           TRUE ~ paste(substring(author,1,1), word(author, -1))
         ))

byAuthor <- bind_rows(byAuthor1, byAuthor2)



write.csv(byAuthor, "cogsci_analysis/cogsci_byAuthor.csv")

byAuthor <- read_csv("cogsci_analysis/cogsci_byAuthor.csv")
# check suffices
byAuthor %>%
  filter(word(author,-1) == "Jr") %>%
  .$author

byAuthor %>%
  filter(year >= 2000) %>%
  pull(authorAbbr) %>%
  unique() %>%
  length()
