library(dplyr)
library(readr)
library(stringr)
setwd("/Users/loey/Desktop/Research/InfluencingCogSci/scraping/cogsci/")
file1 <- read_csv("cogsci_abstracts2000_2013.csv")
file2 <- read_csv("cogsci_abstracts2013-2014.csv")
file3 <- read_csv("cogsci_abstracts2015-2018.csv")
file4 <- read_csv("cogsci_abstracts2019.csv")
fileOrig <- read_csv("cogsci_abstracts_updated.csv")

# test <- read_csv("CogSci_1979-1999/split/1990CogSci.csv", col_names=c("X1","year","authors","title","full_text"))
# glimpse(test)

# file_list <- list.files("/Users/loey/Desktop/Research/InfluencingCogSci/scraping/cogsci/CogSci_1979-1999/split/")
# olderpapers <- data.frame()
# 
# for (i in 1:length(file_list)){
#   this.year = as.numeric(substring(file_list[i],1,4))
#   if(this.year %in% 1982:1989){ #swapped year and title in 1st half of data
#     temp_data <- read_csv(paste0("CogSci_1979-1999/split/",file_list[i]), col_names=c("X1","year","title","authors","full_text"))
#   } else if(this.year %in% 1990:1992){ #swapped year and title in 1st half of data
#     temp_data <- read_csv(paste0("CogSci_1979-1999/split/",file_list[i]), col_names=c("year","title","authors","full_text")) %>%
#       mutate(X1 = 0)
#     if(this.year == 1990){
#       temp_data <- temp_data %>% mutate(year="1990")
#     }
#   } else{
#     temp_data <- read_csv(paste0("CogSci_1979-1999/split/",file_list[i]), col_names=c("X1","year","authors","title","full_text"))
#   }
#   temp_data <- slice(temp_data, 2:nrow(temp_data))
#   olderpapers <- bind_rows(olderpapers, temp_data) #for each iteration, bind the new data to the building dataset
# }
# 
# olderpapers <- olderpapers %>%
#   select(-"X1") %>%
#   mutate(year = as.numeric(year))
# glimpse(olderpapers)

#write.csv(olderpapers, "cogsci_papers_1981-1999.csv")


# #Manual coding to fix formatting errors in authors data
# uneditedAuthors_old <- olderpapers %>%
#   mutate(authors = str_replace_all(authors, c(" and "=", ")),
#          editAuthors = authors) %>%
#   select(year, title, authors, editAuthors)
# write.csv(uneditedAuthors_old, "uneditedAuthors_1981-1999.csv")

#edits <- read_csv("uneditedAuthors_1981-1999_wEdits.csv")

#overwriting authors with manually corrected one--a little risky, just make sure not messing anything up
#olderpapers$authors <- edits$editAuthors 
#write.csv(olderpapers, "cogsci_papers_1981-1999_fixedAuthors.csv")

olderpapers <- read_csv("cogsci_papers_1981-1999_fixedAuthors.csv")

attachOrigAbstracts <- fileOrig %>%
  filter(year %in% 2009:2014) %>%
  select(title,abstract)

duplicates <- bind_rows(file1,file2) %>%
  group_by(year, title) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  .$title

file2_2013 <- file2 %>%
  filter(year == 2013 & !title %in% duplicates)
file2_not2013 <- file2 %>%
  filter(year != 2013)

olderpapers <- olderpapers %>%
  mutate(abstract = NA,
         html_link = NA,
         pdf_link = NA) %>%
  select(-"X1")
allFiles <- bind_rows(file1, file2_2013, file2_not2013, file3, file4, olderpapers)

files2009.2014 <- allFiles %>%
  filter(year %in% 2009:2014) %>%
  select(-abstract) %>%
  left_join(attachOrigAbstracts, by="title")

allFiles <- allFiles %>%
  filter(!year %in% 2009:2014) %>%
  bind_rows(files2009.2014)



splitFullText <- function(text){
  return(unlist(str_split(text, "Abstract", 2))[2])
}

extractAbstract <- function(text){
  if(str_detect(text, "Abstract") & str_detect(text, "Keywords")){
    abstract <- regmatches(text,regexec("Abstract(.*?)Keywords",text))[[1]][2]
  } else if(str_detect(text, "Abstract") & str_detect(text, "Introduction")){
    abstract <- regmatches(text,regexec("Abstract(.*?)Introduction",text))[[1]][2]
  }
  else{
    abstract <- 'NA'
  }
  if(!is.na(abstract) & (str_count(abstract, pattern=" ")+1) > 600){
    abstract <- 'NA'
  }
  return(abstract)
}

extractKeyword <- function(text){
  if(str_detect(text, "Keywords") & str_detect(text, "Introduction")){
    keyword <- regmatches(text,regexec("Keywords(.*?)Introduction",text))[[1]][2]
  } else if(str_detect(text, "Keywords") & str_detect(text, "\n")){
   keyword <- regmatches(text,regexec("Keywords(.*?)\n",text))[[1]][2]
  }
  else{
    keyword <- 'NA'
  }
  if(!is.na(keyword) & (str_count(keyword, pattern=" ")+1) > 50){
    keyword <- 'NA'
  }
  return(keyword)
}

# processing
allFiles <- allFiles %>%
  filter(!title %in% c("Front Matter","Cognitive Science Society title")) %>% #remove full manuscript "papers" from 2000
  mutate(authors = ifelse(is.na(authors), "Michela Balconi", authors)) #manually add in missing only author
nrow(allFiles)
unique(allFiles$year)

# allFiles <- allFiles %>%
#   filter(!title %in% c("Front Matter","Cognitive Science Society title")) %>% # removes full proceedings
#   unique() %>%
#   mutate(authors = ifelse(is.na(authors), "Michela Balconi", authors), # manual input of missing author
#          #abstract = ifelse(is.na(abstract) & !is.na(full_text), mapply(extractAbstract, full_text), abstract),
#          #abstract = str_replace_all(abstract, c("-\n"="", "\n"=" ")),
#          #abstract = trimws(abstract),
#          full_text = mapply(splitFullText, full_text),
#          full_text = str_replace_all(full_text, c("-\n"="", "\n"=" ")),
#          full_text = trimws(full_text))
         #keywords = mapply(extractKeyword, full_text),
         #keywords = str_replace_all(keywords, ":", ""),
         #keywords = trimws(keywords)) %>% # extracts relevant text after first "Abstract" string match
  #filter(!is.na(keywords)) %>%

allFiles %>%
  filter(year == 2019) %>%
  head(100)

write.csv(allFiles, "cogsci_papers.csv")
