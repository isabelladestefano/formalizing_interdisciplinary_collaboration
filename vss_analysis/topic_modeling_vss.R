library(stm) 
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
library(wordcloud)
library(factoextra)
library(reshape2)
library(cowplot)
library(gganimate)
library(forcats)
library(ggdendro)
library(seewave) # K-L distance

setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
#source("topic_labels.R")

# Resources:
# http://www.structuraltopicmodel.com/


############################
### PROCESSING FUNCTIONS ###
############################

# NB: none of these are currently used in the code below but have been previously helpful so leaving them here

clean_abstracts <- function(data_frame) {
  # Clean abstract column (expects a data frame with an `abstract` column)
  # Removes punctuation and escape characters, "\\n", "\\t", "\\f".
  # Creates exception for words containing punctuation, "e.g." & "i.e."
  data_frame$abstract <- as.character(data_frame$abstract, na.omit = T)
  data_frame <- data_frame %>%
    mutate(abstract_cleaned = str_replace_all(abstract, c("e\\.g\\." = "e1g1", "i\\.e\\." = "i1e1")),
           abstract_cleaned = str_replace_all(abstract_cleaned, c("[^a-zA-Z0-9\\&\\s]" = " ", "[\\n\\t\\f]" = "")),
           abstract_cleaned = str_replace_all(abstract_cleaned, c("e1g1" = "e.g.", "i1e1" = "i.e.")))
  return(data_frame)
}

structure_text <- function(documents, metadata = NA) {
  print("Processing documents")
  if (!is.na(metadata)) {
    processed <- textProcessor(documents, metadata = metadata) 
  } else { 
    processed <- textProcessor(documents)
  }
  
  print("Preparing documents for modeling")
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)
  
  return(out)
}



#################
### FIT MODEL ###
#################

DATA = "vss_abstracts_all.csv"
TOPICS = 100 # NB: tweak this to have changes reflected in all code below

df.fulltext <- read_csv(DATA, col_types = cols(abstract=col_character())) %>%
  select(-"X1")


# textProcessor function supplied by stm
processed <- textProcessor(df.fulltext$abstract)

# Catch papers removed during textProcessor call above
removed <- processed$docs.removed
fulltext.papers.cleaned = df.fulltext[-removed,]
dim(df.fulltext)
dim(fulltext.papers.cleaned)

# prepDocuments function supplied by stm
fulltext.model.framework <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)

# Catch second round of papers removed during prepDocuments
removed.model = fulltext.model.framework$docs.removed
fulltext.papers.cleaned.model = fulltext.papers.cleaned[-removed.model,]
dim(fulltext.papers.cleaned.model)

# Fit model: can take 20+ mins. for 50 topics or more
fulltext.model.manual <- stm(documents = fulltext.model.framework$documents, 
                             vocab = fulltext.model.framework$vocab,
                             K = TOPICS,
                             max.em.its = 200, # tweak this param as needed
                             init.type = "Spectral") # default "Spectral", note can also use "LDA" here for Gibbs sampler instead of variational inference

# k = 10 converges after 33 iterations
# k = 25 converges after 76 iterations
# k = 50 converges after 81 iterations
# k = 100 converges after 78 iterations but take multiple hours


# Summary of model
summary(fulltext.model.manual)
#summary.STM(fulltext.model.manual) #seems broken

##################
### SAVE MODEL ###
##################

# Save model as RData
model.rdata = paste('model_', TOPICS, '.RData', sep = '')
save(fulltext.model.manual, file = model.rdata)

# Save model output as csv
topic.dist = fulltext.model.manual$theta # Number of Documents by Number of Topics matrix of topic proportions.

df.papers = data.frame(title = df.fulltext$title,
                       authors = df.fulltext$authors,
                       year = df.fulltext$year)
df.topic.dist = cbind(df.papers, topic.dist)

# Write to csv for processing elsewhere
csv.title = paste('vss_topic_dist_fulltext_', TOPICS, '.csv', sep = '')
write.csv(df.topic.dist, csv.title)

# test csv
topics = read_csv("vss_topic_dist_fulltext_100.csv")

nrow(topics)
glimpse(topics)

#######################
### VISUALIZE MODEL ###
#######################

# Validate model
#topics = labelTopics(fulltext.model.manual)
#save(topics, file = paste('topic_list.', TOPICS, '.RData', sep = ''))

# Word cloud of topics above
cloud(stmobj = fulltext.model.manual,
      topic = 3, # Replace this with relevant topic from list above
      type = "model",
      max.words = 10) # word cloud of most probable 10 words in topic param


# Visualizing model via functions provided by stm package
topicQuality(fulltext.model.manual, fulltext.papers.cleaned)

fulltext.model.manual$runout










# Analysis by Year
topicsByYear <- topics %>%
  group_by(year) %>%
  select(-c(title, authors)) %>%
  summarise_all(mean) %>%
  gather("topic","score", 2:101)

trends.meta <- topicsByYear %>%
  filter(topicType == "meta") %>%
  ggplot(aes(x=year, y=score, colour=topic)) +
  geom_smooth(size=0.25, se=FALSE) +
  scale_y_continuous(limits=c(0, 0.055))

trends.measure <- topicsByYear %>%
  filter(topicType == "measure") %>%
  ggplot(aes(x=year, y=score, colour=topic)) +
  geom_smooth(size=0.25, se=FALSE) +
  scale_y_continuous(limits=c(0, 0.05))

trends.model <- topicsByYear %>%
  filter(topicType == "model") %>%
  ggplot(aes(x=year, y=score, colour=topic)) +
  geom_smooth(size=0.25, se=FALSE) +
  scale_y_continuous(limits=c(0, 0.05))

trends.language <- topicsByYear %>%
  filter(topicType == "language") %>%
  ggplot(aes(x=year, y=score, colour=topic)) +
  geom_smooth(size=0.25, se=FALSE) +
  scale_y_continuous(limits=c(0, 0.05))
  
trends.concepts <- topicsByYear %>%
  filter(topicType == "concepts") %>%
  ggplot(aes(x=year, y=score, colour=topic)) +
  geom_smooth(size=0.25, se=FALSE) +
  scale_y_continuous(limits=c(0, 0.05))

trends.decision <- topicsByYear %>%
  filter(topicType == "decision") %>%
  ggplot(aes(x=year, y=score, colour=topic)) +
  geom_smooth(size=0.25, se=FALSE) +
  scale_y_continuous(limits=c(0, 0.05))

trends.social <- topicsByYear %>%
  filter(topicType == "social") %>%
  ggplot(aes(x=year, y=score, colour=topic)) +
  geom_smooth(size=0.25, se=FALSE) +
  scale_y_continuous(limits=c(0, 0.05))

trends.cognitive <- topicsByYear %>%
  filter(topicType == "cognitive") %>%
  ggplot(aes(x=year, y=score, colour=topic)) +
  geom_smooth(size=0.25, se=FALSE) +
  scale_y_continuous(limits=c(0, 0.05))

plot_grid(trends.meta, trends.measure, trends.model, trends.language, trends.concepts, trends.decision, trends.social, trends.cognitive, nrow=4)
ggsave("img/topicTrends.png")

# arrange by variance/means across years
topicsByYear.summ <- topicsByYear %>%
  group_by(topic) %>%
  summarise(mean = mean(score),
            sd = sd(score)) %>%
  arrange(desc(sd))
overall.mean <- topicsByYear.summ %>%
  summarise(mean=mean(mean)) %>%
  .$mean

topicsByYear %>%
  left_join(topicsByYear.summ, by="topic") %>%
  mutate(topic = fct_reorder(topic, desc(sd))) %>%
  ggplot(aes(x=topic, y=score, fill=topic)) +
  geom_boxplot() +
  geom_hline(yintercept = overall.mean, colour="red", linetype=2) +
  scale_fill_discrete(guide="none") +
  scale_y_continuous("mean score by year") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6))
ggsave("img/topicsByYear_descSD_box_100.png")


topicsByYear %>%
  left_join(topicsByYear.summ, by="topic") %>%
  mutate(topic = fct_reorder(topic, desc(mean))) %>%
  ggplot(aes(x=topic, y=score, fill=topic)) +
  geom_boxplot() +
  geom_hline(yintercept = overall.mean, colour="red", linetype=2) +
  scale_fill_discrete(guide="none") +
  scale_y_continuous("mean score by year") +
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6))
ggsave("img/topicsByYear_descMean_box_100.png")

#binning by variance trends
trends_binVariance <- function(dectile){
  selectTopics <- slice(topicsByYear.summ, (dectile+1):(dectile+10)) %>%
    .$topic
  topicsByYear %>%
    filter(topic %in% selectTopics) %>%
    ggplot(aes(x=year, y=score, colour=topic)) +
    geom_smooth(size=0.25, se=FALSE) +
    theme(legend.text = element_text(size = 6))
}

plot_grid(trends_binVariance(0),
          trends_binVariance(10),
          trends_binVariance(20),
          trends_binVariance(30),
          trends_binVariance(40),
          trends_binVariance(50),
          trends_binVariance(60),
          trends_binVariance(70),
          trends_binVariance(80),
          trends_binVariance(90),
          nrow=4
)
ggsave("img/topicTrends.100_byVar.png")


# Author analysis

centrality <- read_csv("all_centrality.csv")
centrality_allYears <- NULL
for(i in 2000:2019){
  filename = paste0("centrality_byYear/centrality_",i,".csv")
  new.centrality <- read_csv(filename)
  new.centrality <- new.centrality %>%
    mutate(year=i)
  centrality_allYears <- bind_rows(centrality_allYears, new.centrality)
}
centrality_allYears

# Variance of centrality values by year
centrality_allYears %>%
  group_by(year, CM) %>%
  summarise(variance = var(measure)) %>%
  ggplot(aes(x=year, y=variance, colour=CM)) +
  geom_line() +
  scale_colour_discrete(guide="none") +
  facet_wrap(~CM, scales="free")


centrality_allYears <- centrality_allYears %>%
  mutate(authorAbbr = label) %>%
  select(-c("X1","id","label"))
between <- centrality_allYears %>%
  filter(CM == "between") %>%
  mutate(between = measure) %>%
  select(-c("measure","CM"))
close <- centrality_allYears %>%
  filter(CM == "close") %>%
  mutate(close = measure) %>%
  select(-c("measure","CM"))
degree <- centrality_allYears %>%
  filter(CM == "degree") %>%
  mutate(degree = measure) %>%
  select(-c("measure","CM"))
eigen <- centrality_allYears %>%
  filter(CM == "eigen") %>%
  mutate(eigen = measure) %>%
  select(-c("measure","CM"))

editAuthors <- function(origAuthors){
  temp <- matrix(strsplit(origAuthors, ",")[[1]], , 2, byrow=TRUE)
  author <- paste(trimws(temp[,2]), trimws(temp[,1]), sep=" ")
  return(author)
}
byAuthor1_topics <- topics %>%
  filter(year <= 2014) %>%
  mutate(author = mapply(editAuthors, authors)) %>%
  unnest(author) %>%
  mutate(authorAbbr = paste(substring(author,1,1), word(author, -1)))
byAuthor2_topics <- topics %>%
  filter(year > 2014) %>%
  mutate(authors=stri_trans_general(authors, "latin-ascii"),
         author=str_replace_all(authors, ",$", ""),
         author=str_replace_all(author, c(".*\n"="", " *\\(.*?\\)"="", "  "=" ")),
         author=str_replace_all(author, " & ", ", "),
         author=strsplit(author, ", ")) %>%
  unnest(author) %>%
  filter(!grepl("University|Institute|Center|Centre|Centro|School|Department|Dept|Unit|Hospital|\\d",
                author)) %>%
  mutate(author=trimws(author),
         authorAbbr = paste(substring(author,1,1), word(author, -1)))
byAuthor_topics <- bind_rows(byAuthor1_topics, byAuthor2_topics)

byAuthor_topics_central <- left_join(byAuthor_topics, between, by=c("authorAbbr","year"))
byAuthor_topics_central <- left_join(byAuthor_topics_central, close, by=c("authorAbbr","year"))
byAuthor_topics_central <- left_join(byAuthor_topics_central, degree, by=c("authorAbbr","year"))
byAuthor_topics_central <- left_join(byAuthor_topics_central, eigen, by=c("authorAbbr","year"))

View(byAuthor_topics_central)


last.names <- byAuthor_topics %>%
  mutate(lastname = word(author, -1)) %>%
  group_by(authors, title, year) %>%
  summarise(authors.lastname = paste(lastname, collapse=", "))

byAuthor_topics <- byAuthor_topics %>%
  left_join(last.names, by=c("authors", "title", "year"))
# Clustering
n.chars = 60
searchAuthor <- function(AUTHOR.SEARCH){
  topics.authorSel <- byAuthor_topics %>%
    mutate(authors.year = paste0(authors.lastname, " (", year, ")"))
  if(grepl("Year_", AUTHOR.SEARCH)){
    topics.authorSel <- filter(topics.authorSel, year == as.numeric(substring(AUTHOR.SEARCH,nchar(AUTHOR.SEARCH)-3,nchar(AUTHOR.SEARCH))))
  } else if(AUTHOR.SEARCH != "all"){
    topics.authorSel <- filter(topics.authorSel, authorAbbr == AUTHOR.SEARCH)
  }
  repeats <- topics.authorSel %>%
    group_by(authors.year) %>%
    summarise(count = n()) %>%
    filter(count > 1) %>%
    arrange(desc(count)) %>%
    mutate(incrCount = 1)
  for(i in 1:length(topics.authorSel$authors.year)){
    if(topics.authorSel$authors.year[i] %in% repeats$authors.year){
      repeats.row = match(topics.authorSel$authors.year[i], repeats$authors.year)
      topics.authorSel$authors.year[i] = paste0(topics.authorSel$authors.year[i], letters[filter(repeats, authors.year==topics.authorSel$authors.year[i])$incrCount])
      repeats[repeats.row,]$incrCount = repeats[repeats.row,]$incrCount + 1
    }
    if(nchar(topics.authorSel$authors.year[i]) > n.chars){
      num.chars = nchar(topics.authorSel$authors.year[i])
      topics.authorSel$authors.year[i] = paste0(substr(topics.authorSel$authors.year[i], 1, n.chars-18),"...",substr(topics.authorSel$authors.year[i], num.chars-15, num.chars))
    }
  }
  row.names(topics.authorSel) <- topics.authorSel$authors.year
  topics.authorSel <- topics.authorSel %>%
    mutate(authors = authors.year) %>%
    select(-c("title", "year", "nonsense", "cid", "cogsci-publishing", "authorAbbr", "author", "authors.lastname", "authors.year"))
  row.names(topics.authorSel) <- topics.authorSel$authors
  topics.authorSel <- topics.authorSel %>%
    select(-"authors")
  return(topics.authorSel)
}

dendroAuthor <- function(AUTHOR.SEARCH){
  topics.authorSel <- searchAuthor(AUTHOR.SEARCH)
  hc <- hclust(dist(topics.authorSel), "ave")
  dendr <- ggdendrogram(hc, rotate = TRUE, size = 2)
  print(paste("# of papers by author:",nrow(topics.authorSel)))
  if(nrow(topics.authorSel) > 80){
    dendr = dendr + theme(axis.text.y = element_text(size = 4))
  }
  ggsave(paste0("img/authorDendrograms/dendrogram_",AUTHOR.SEARCH,".png"), dendr)
  dendr
}

dendroAuthor("E Vul")

topics.author.t <- searchAuthor("all") %>%
  t()
hct <- hclust(dist(topics.author.t), "ave")
ggdendrogram(hct, rotate = TRUE, size = 2)

#2D trends

byAuthor_topics_central %>%
  filter(!is.na(eigen)) %>%
  gather("topic","score", 4:53) %>%
  filter(!topic %in% c("nonsense", "cid","cogsci-publishing")) %>%
  group_by(year) %>%
  select(-c(title, authors, between, close, degree)) %>%
  mutate(value = eigen*score) %>%
  ungroup() %>%
  group_by(year, topic) %>%
  summarise(weighted.eigen = mean(value)) %>%
  arrange(desc(weighted.eigen)) %>%
  ggplot(aes(x=year, y=topic, fill=weighted.eigen)) +
  geom_tile() +
  scale_fill_gradientn(colours = rainbow(7))

#animated version

anim <- byAuthor_topics_central %>%
  filter(!is.na(eigen)) %>%
  mutate(year = as.integer(year)) %>%
  gather("topic","score", 4:53) %>%
  filter(!topic %in% c("nonsense", "cid","cogsci-publishing")) %>%
  group_by(year) %>%
  select(-c(title, authors, between, close, degree)) %>%
  mutate(value = eigen*score) %>%
  ungroup() %>%
  group_by(year, topic) %>%
  summarise(weighted.eigen = mean(value)) %>%
  arrange(desc(weighted.eigen)) %>%
  ggplot(aes(x=weighted.eigen, y=topic, colour=topic)) +
  geom_point() +
  ggtitle('Year: {frame_time}') +
  scale_colour_discrete(guide="none") +
  scale_x_continuous(expand=c(0,0,0,0.0005)) +
  transition_time(year) +
  theme_bw()

animate(anim)


glimpse(byAuthor_topics_central)








# KL Divergence
TOPICS = 100
topics = read_csv(paste0("vss_topic_dist_fulltext_",TOPICS,".csv")) %>%
  select(-"X1")
topics <- topics %>%
  distinct()
topicsByYear <- topics %>%
  group_by(year) %>%
  select(-c(title, authors)) %>%
  summarise_all(mean) %>%
  gather("topic","mean_score", 2:(TOPICS+1)) 

allKLDist <- data.frame()
for(i in 2001:2018){
  this.year = i
  next.year = i+1
  this.matr = topicsByYear %>%
    filter(year==this.year) %>%
    mutate(topic=as.numeric(topic)) %>%
    select(-"year") %>%
    as.matrix()
  next.matr = topicsByYear %>%
    filter(year==next.year) %>%
    mutate(topic=as.numeric(topic)) %>%
    select(-"year") %>%
    as.matrix()
  dist = kl.dist(this.matr, next.matr)
  temp = data.frame(year = this.year,
                    D1 = dist$D1,
                    D2 = dist$D2,
                    D = dist$D)
  allKLDist = bind_rows(allKLDist, temp)
}
ggplot(allKLDist, aes(x=year, y=D)) +
  geom_line() +
  ggtitle(paste(TOPICS,"topics for VSS"))
ggsave(paste0("img/KLDist",TOPICS,".png"))

