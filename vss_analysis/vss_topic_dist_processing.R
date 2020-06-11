### Script / functions for processing topic distributions over a set of papers
library(tidyverse)
library(pbapply)
setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
source('networkCentralityFunctions_vss.R')

# DATA_100 = 'vss_topic_dist_fulltext_100.csv'
# topic.df = read_csv(DATA_100) %>%
#   select(-"X1")
# glimpse(topic.df)

#topic.df = transform_authorAbbr(topic.df) #this takes a long time to run so read_csv if possibles
#write.csv(topic.df, "vss_topics_authorAbbr.csv")
topic.df <- read_csv("vss_topics_authorAbbr.csv") %>%
  select(-"X1") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors))

sanitycheck_author_rows <- function(author.lookup, df) {
  author.grep = as.vector(unlist(sapply(author.lookup, function(author.lookup){return(grep(author.lookup, df$authors, value = TRUE))})))
  unique(author.grep)
}

glimpse(topic.df)
length(unique(topic.df$authors))


# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
  return(topic.means)
}

global.means = get_avg_topic_dist(topic.df) #keep in mind duplicate papers depending on author and should collapse over these






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

# define years.df before running
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

# test cosine similarity
years.df <- selectYears(2016, 1)
selectTopicByAuthor("J Tenenbaum")
authors_prevTopicSim("J Wolfe", "T Brady")
TEST.YEARS = 2001:2020
TEST.AUTHORS = c("J Tenenbaum","T Brady","J Wolfe","E Vul", "J Fan", "H Schill")
TEST.N.YEARS = 1
years.df <- selectYears(2014, TEST.N.YEARS)
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
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    .$authors
  full.author <- expand.grid(authorA=ALL.AUTHORS, authorB=ALL.AUTHORS) #repeat data frame for each year
  topicSim.df <- full.author %>%
    mutate(authorsSim = pbmapply(authors_prevTopicSim, authorA, authorB))
  filename = paste0("topicSimYear/vss_topicSim_",year,".csv")
  write_csv(topicSim.df, filename)
}
