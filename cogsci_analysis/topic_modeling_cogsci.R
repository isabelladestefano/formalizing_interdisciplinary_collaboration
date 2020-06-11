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

setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
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

DATA = "cogsci_papers.csv"
TOPICS = 100 # NB: tweak this to have changes reflected in all code below

df.fulltext <- read_csv(DATA)

# textProcessor function supplied by stm
processed <- textProcessor(df.fulltext$full_text)
#processed <- textProcessor(df.fulltext$abstract)

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

df.papers = data.frame(title = fulltext.papers.cleaned$title,
                       authors = fulltext.papers.cleaned$authors,
                       year = fulltext.papers.cleaned$year)

nrow(fulltext.papers.cleaned)
nrow(df.fulltext)
nrow(topic.dist)
df.topic.dist = cbind(df.papers, topic.dist)

# Write to csv for processing elsewhere
csv.title = paste('cogsci_topic_dist_fulltext_', TOPICS, '.csv', sep = '')
write_csv(df.topic.dist, csv.title)


# remove repeated rows
topics <- topics %>%
  distinct()


#names(topics)[4:53] <- paper.topic.names #for 50 topics
names(topics)[4:103] <- paper.topic.names.100 #for 100 topics

glimpse(topics)

#######################
### VISUALIZE MODEL ###
#######################

# Validate model
#topics = labelTopics(fulltext.model.manual)
#save(topics, file = paste('topic_list.', TOPICS, '.RData', sep = ''))

# Word cloud of topics above
cloud(stmobj = fulltext.model.manual,
      topic = 31, # Replace this with relevant topic from list above
      type = "model",
      max.words = 25) # word cloud of most probable 10 words in topic param



