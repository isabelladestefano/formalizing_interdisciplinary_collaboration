### Script / functions for processing topic distributions over a set of papers
setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(pbapply)
source('networkCentralityFunctions_cogsci.R')

DATA_100 = 'cogsci_topic_dist_fulltext_100.csv'
topic.df = read_csv(DATA_100)
topic.df = transform_authorAbbr(topic.df) #this takes a long time to run so read_csv if possible
write.csv(topic.df, "cogsci_topics_authorAbbr.csv")

topic.df <- read_csv("cogsci_topics_authorAbbr.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors),
         authors=ifelse(authors=="J Tenenbaums", "J Tenenbaum", authors)) %>% #manually fixing incorrect author naming
  dplyr::select(-X1)
sanitycheck_author_rows <- function(author.lookup, df) {
  author.grep = as.vector(unlist(sapply(author.lookup, function(author.lookup){return(grep(author.lookup, df$authors, value = TRUE))})))
  unique(author.grep)
}

topic.author.year <- topic.df %>%
  group_by(authors, year) %>%
  select(-title) %>%
  summarise_all(mean)
head(topic.author.year,20)

glimpse(topic.author.year)


length(unique(topic.author.year$authors))

# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    dplyr::select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
  return(topic.means)
}

# Similarity between authors
magnitude <- function(a){
  sqrt(sum(a^2))
}

cosine_similarity <- function(x, y){
  dot.product = sum(x*y)
  mag.x = magnitude(x)
  mag.y = magnitude(y)
  return(dot.product/(mag.x*mag.y))
}

get_projection_angle = function(vec.a, vec.b) { #not using this
  # get projection angles
  theta = acos(pmin(cosine_similarity(vec.a, vec.b),1)) # cos^-1 to get theta/angle in radians, pmin deals with NaN issues from acos()
  return(theta)
}

# define years.df and before running
selectYears <- function(currYear, n.prevYears){
  topic.df %>%
    filter(year %in% (currYear-n.prevYears+1):(currYear))
}

selectTopicByAuthor <- function(author){
  years.df %>%
    filter(authors == author) %>%
    get_avg_topic_dist()
}

authors_prevTopicSim <- function(author.a, author.b){
  topic.a = selectTopicByAuthor(author.a)
  topic.b = selectTopicByAuthor(author.b)
  return(get_projection_angle(topic.a$topic.means, topic.b$topic.means))
}






global.means = get_avg_topic_dist(topic.df)








# test cosine similarity
years.df <- selectYears(2000, 1)
selectTopicByAuthor("E Vul")
authors_prevTopicSim("E Vul", "E Brockbank")
TEST.YEARS = 2001:2020
TEST.AUTHORS = c("E Vul","J Fan","D Barner","A Schachner", "C Walker", "E Brockbank")
TEST.N.YEARS = 2
years.df <- selectYears(2017, TEST.N.YEARS)
test.df <- data.frame(author.a = TEST.AUTHORS, length(TEST.AUTHORS),
                      author.b = rep(TEST.AUTHORS, each=length(TEST.AUTHORS))) %>%
  mutate(authorsSim = pbmapply(authors_prevTopicSim, author.a, author.b))

matrix(round(test.df$authorsSim,4),
       nrow=length(TEST.AUTHORS),
       ncol=length(TEST.AUTHORS),
       dimnames=list(TEST.AUTHORS, TEST.AUTHORS))



# unique authors arranged by prolificity
mostPublished <- topic.df %>%
  group_by(authors) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  .$authors

nrow(topic.df)
ALL.AUTHORS = unique(topic.df$authors)
#ALL.AUTHORS = mostPublished
#ALL.AUTHORS = TEST.AUTHORS
length(ALL.AUTHORS)

N.YEARS = 1
ALL.YEARS = (min(topic.df$year)+N.YEARS-1):(max(topic.df$year))
for(year in ALL.YEARS){
  years.df <- selectYears(year, N.YEARS)
  ALL.AUTHORS = years.df %>%
    group_by(authors) %>%
    select(authors) %>%
    distinct() %>%
    arrange(authors) %>%
    .$authors
  auth.matr <- matrix(1,nrow=length(ALL.AUTHORS), ncol=length(ALL.AUTHORS))
  colnames(auth.matr) <- ALL.AUTHORS
  auth.matr[upper.tri(auth.matr, diag=TRUE)] <- NA
  full.author <- auth.matr %>%
    as.data.frame() %>%
    gather("authorA","val",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(auth.matr),length(colnames(auth.matr)))) %>%
    filter(!is.na(val)) %>%
    select(-val)
  topicSim.df <- full.author %>%
    mutate(authorsSim = pbmapply(authors_prevTopicSim, authorA, authorB))
  filename = paste0("topicSimYear/cogsci_topicSim_",year,".csv")
  write_csv(topicSim.df, filename)
}






# Run with 5 years span of papers (e.g. similarity of all papers in last 5 years)

# N.YEARS = 5
# ALL.YEARS = (min(topic.df$year)+N.YEARS-1):(max(topic.df$year))
# for(year in ALL.YEARS){
#   years.df <- selectYears(year, N.YEARS)
#   ALL.AUTHORS = years.df %>%
#     group_by(authors) %>%
#     select(authors) %>%
#     distinct() %>%
#     arrange(authors) %>%
#     .$authors
#   auth.matr <- matrix(1,nrow=length(ALL.AUTHORS), ncol=length(ALL.AUTHORS))
#   colnames(auth.matr) <- ALL.AUTHORS
#   auth.matr[upper.tri(auth.matr, diag=TRUE)] <- NA
#   full.author <- auth.matr %>%
#     as.data.frame() %>%
#     gather("authorA","val",1:ncol(.)) %>%
#     mutate(authorB=rep(colnames(auth.matr),length(colnames(auth.matr)))) %>%
#     filter(!is.na(val)) %>%
#     select(-val)
#   topicSim.df <- full.author %>%
#     mutate(authorsSim = pbmapply(authors_prevTopicSim, authorA, authorB))
#   filename = paste0("topicSimYear.5/cogsci_topicSim.5_",year,".csv")
#   write_csv(topicSim.df, filename)
# }






