setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
library(tidyverse)
library(lme4)
library(futile.matrix)
vss_byAuthor = read_csv("vss_byAuthor.csv")

fullauthor_key = unique(vss_byAuthor$authorAbbr)
fullauthor_key
length(fullauthor_key)


i=1
coauthor_mats = list()
topicSims = list()

for(year in 2001:2019){
  load(paste0("vss_binary/author_mat_year_vss_", year,".RData"))
  coauthor_mats[[i]] = author_mat_year
  names(coauthor_mats[[i]]) <-NULL 
  topicSims[[i]] = read_csv(paste0("topicSimYear/vss_topicSim_", year,".csv"))
  i = i+1
}

length(topicSims) == length(coauthor_mats)

isSymmetric(coauthor_mats[[1]])


### Old Functions ###
makeSym = function(mat){
  mat[upper.tri(mat, diag = TRUE)] <- t(mat)[upper.tri(mat, diag = TRUE)]
  return(mat)
}

pmax.matr <- function(matr){ #function to make matrix symmetrical (pmax)
  tri <- pmax(matr[lower.tri(matr, diag=FALSE)], t(matr)[lower.tri(matr, diag=FALSE)]) #pmax of triangels
  matr[lower.tri(matr, diag = FALSE)] <- tri
  matr[upper.tri(matr, diag=FALSE)] <- t(matr)[upper.tri(matr, diag = FALSE)]
  return(matr)
}

#################


length(topicSims) #should be 20


peek(coauthor_mats[[1]], 15)
head(topicSims[[1]],15)



# sanity check
vss_byAuthor %>%
  filter(year==2001) %>%
  .$authorAbbr %>%
  unique() %>%
  length()
topicSims[[1]] %>%
  nrow() %>%
  sqrt()
# should be equal for each year
# sanity check coauthorship matrix

test <- matrix(floor(runif(16, 0, 10)), nrow=4, ncol=4)
test
pmax.matr(test)

dim(coauthor_mats[[1]])
length(author_key)



#### Combine Topic Similarity with Co-Authorship Matrix for n-1 Year ####

topic.coauthor.matrices <- list()
years = 2001:2019

start <- Sys.time()
for(i in 1:length(topicSims)){
  print(paste0("year: ",years[i]))
  author_key = vss_byAuthor %>% filter(year  == years[i]) %>% pull(authorAbbr) %>% unique()
  
  tempTop <- topicSims[[i]]
  tempTop <- tempTop %>%
    spread(authorB, authorsSim) %>%
    dplyr::select(-authorA) %>%
    as.matrix()
  tempTop[upper.tri(tempTop, diag = TRUE)] <- NA #ignore upper triangle
  #peek(round(tempTop,4),15)

  tempAuth <- coauthor_mats[[i]]
  #tempAuth <- pmax.matr(tempAuth)
  colnames(tempAuth) <- author_key
  #peek(tempAuth, 15)

  tempTop.df <- tempTop %>%
    as.data.frame() %>%
    gather("authorA","topicSim",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(tempTop),length(colnames(tempTop)))) %>%
    # mutate(authorB=rep(colnames(tempTop),length(colnames(tempTop))),
    #        tempA = ifelse(authorA < authorB, authorA, authorB), #alphabetically first author => authorA
    #        tempB = ifelse(authorA > authorB, authorA, authorB), #alphabetically last author => authorB
    #        authorA = tempA, 
    #        authorB = tempB) %>%
    # dplyr::select(-c(tempA, tempB)) %>%
    filter(!is.na(topicSim))
  tempAuth.df <- tempAuth %>%
    as.data.frame() %>%
    gather("authorA","prior_publication",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(tempAuth),length(colnames(tempAuth)))) %>%
    filter(!is.na(prior_publication))
  tempBoth <- tempTop.df %>%
    left_join(tempAuth.df, by=c("authorA","authorB")) #this takes the longest

  topic.coauthor.matrices[[i]] <- tempBoth %>%
    mutate(year=years[i]) %>%
    distinct()
  print(Sys.time()-start)
}

head(topic.coauthor.matrices[[1]],15)

# 
# # bind dependent measure (whether authors publish together the following year)
start <- Sys.time()
for(i in 1:(length(topicSims)-1)){
  print(paste0("year: ",years[i]))
  temp <- topic.coauthor.matrices[[i]]
  temp.next <- topic.coauthor.matrices[[i+1]] %>%
    mutate(new_publication = prior_publication) %>%
    dplyr::select(-c(prior_publication, topicSim, year))
  temp.tot <- temp %>%
    left_join(temp.next, by=c("authorA","authorB")) %>%
    mutate(new_publication = ifelse(is.na(new_publication), 0, new_publication))
  topic.coauthor.matrices[[i]] <- temp.tot
  print(Sys.time()-start)
}

for(i in 1:length(topicSims)){
  write.csv(topic.coauthor.matrices[[i]], paste0("topicCoauthMatr/topicCoauth",years[i],".csv"))
}






#########################################################################


#### Combine Topic Similarity with Co-Authorship Matrix for n-5 Years ####
topic.coauthor.matrices.5 <- list()
years = 2001:2019
start <- Sys.time()
for(i in 5:length(topicSims)){
  print(paste0("year: ",years[i]))
  tempTop <- topicSims[[i]]
  tempTop <- tempTop %>%
    spread(authorB, authorsSim) %>%
    dplyr::select(-authorA) %>%
    as.matrix()
  tempTop[upper.tri(tempTop, diag = TRUE)] <- NA #ignore upper triangle
  tempTop.df <- tempTop %>%
    as.data.frame() %>%
    gather("authorA","topicSim",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(tempTop),length(colnames(tempTop)))) %>%
    filter(!is.na(topicSim))
  
  x = 1
  for(j in i:(i-4)){
    author_key = vss_byAuthor %>% filter(year== years[j]) %>% pull(authorAbbr) %>% unique()
    tempAuth <- coauthor_mats[[j]]
    colnames(tempAuth) <- author_key
    #peek(tempAuth, 15)
    
    tempAuth.df <- tempAuth %>%
      as.data.frame() %>%
      gather("authorA","prior_publication",1:ncol(.)) %>%
      mutate(authorB=rep(colnames(tempAuth),length(colnames(tempAuth))))
    names(tempAuth.df) <- c("authorA",paste0("prior_publication_minus",x),"authorB")
    tempTop.df <- tempTop.df %>%
      left_join(tempAuth.df, by=c("authorA","authorB"))
    x = x + 1
  }
  
  # append new_publication, for all years except most recent year
  if(i < length(topicSims)){
    author_key = vss_byAuthor %>% filter(year== years[i+1]) %>% pull(authorAbbr) %>% unique()
    tempAuth <- coauthor_mats[[i+1]]
    colnames(tempAuth) <- author_key
    
    tempAuth.df <- tempAuth %>%
      as.data.frame() %>%
      gather("authorA","new_publication",1:ncol(.)) %>%
      mutate(authorB=rep(colnames(tempAuth),length(colnames(tempAuth))))
    
    tempTop.df <- tempTop.df %>%
      left_join(tempAuth.df, by=c("authorA","authorB")) %>%
      mutate(new_publication = ifelse(is.na(new_publication), 0, new_publication))
  }
  
  topic.coauthor.matrices.5[[i-4]] <- tempTop.df
  print(Sys.time()-start)
}
length(topic.coauthor.matrices.5)
head(topic.coauthor.matrices.5[[1]],25)


for(i in 1:length(topic.coauthor.matrices.5)){
  write.csv(topic.coauthor.matrices.5[[i]], paste0("topicCoauthMatr.5/topicCoauth",years[i+4],".5.csv"))
}












