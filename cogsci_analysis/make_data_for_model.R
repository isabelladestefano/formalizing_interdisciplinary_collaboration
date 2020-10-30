library(tidyverse)
library(lme4)
library(futile.matrix)
cogsci_byAuthor = read_csv("cogsci_byAuthor.csv")
author_key = unique(cogsci_byAuthor$authorAbbr)
author_key
length(author_key)

i=1
coauthor_mats = list()
topicSims = list()
for(year in 2000:2019){
  load(paste0("cogsci_binary/author_mat_year_cogsci_", year,".RData")) #generated in author_matrix_by_year.R
  coauthor_mats[[i]] = author_mat_year
  names(coauthor_mats[[i]]) <-NULL 
  topicSims[[i]] = read_csv(paste0("topicSimYear/cogsci_topicSim_", year,".csv"))
  i = i+1
}




# check same number of years for both data frames
length(topicSims) == length(coauthor_mats)
topicSims[[1]]
# check same number of unique authors for both data frames
years=2000:2019
for(i in 1:length(topicSims)) {
  tempTopic = topicSims[[i]] %>%
    gather("ab","author",1:2) %>%
    pull(author) %>%
    unique()
  tempTopic.len = length(tempTopic)
  tempAuth.len = dim(coauthor_mats[[i]])[1]
  print(paste(1999+i,":",tempTopic.len == tempAuth.len))
  if(tempTopic.len != tempAuth.len){
    tempAuth = cogsci_byAuthor %>% filter(year== years[i]) %>% pull(authorAbbr) %>% unique()
    print(setdiff(tempTopic, tempAuth))
  }
}

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


peek(coauthor_mats[[20]], 5)
head(topicSims[[20]],5)



# sanity check
cogsci_byAuthor %>%
  filter(year==2000) %>%
  .$authorAbbr %>%
  unique() %>%
  length()
topicSims[[1]] %>%
  filter(authorB == "V Sloutsky") %>%
  nrow()
# should be equal for each year
# sanity check coauthorship matrix

test <- matrix(floor(runif(16, 0, 10)), nrow=4, ncol=4)
test

pmax.matr(test)


#### Combine Topic Similarity with Co-Authorship Matrix for n-1 Year ####

topic.coauthor.matrices <- list()
years = 2000:2019
start <- Sys.time()
i=1
for(i in 1:length(topicSims)){
  print(paste0("year: ",years[i]))
  author_key = cogsci_byAuthor %>% filter(year== years[i]) %>% pull(authorAbbr) %>% unique()
  
  tempTop.df <- topicSims[[i]]
  
  tempAuth <- coauthor_mats[[i]]
  colnames(tempAuth) <- author_key
  #peek(tempAuth, 15)
  
  tempAuth.df <- tempAuth %>%
    as.data.frame() %>%
    gather("authorA","prior_publication",1:ncol(.)) %>%
    mutate(authorB=rep(colnames(tempAuth),length(colnames(tempAuth)))) %>%
    filter(!is.na(prior_publication))
  topic.coauthor.matrices[[i]] <- tempTop.df %>%
    left_join(tempAuth.df, by=c("authorA","authorB")) %>%
    mutate(year=years[i]) %>%
    distinct()
  print(Sys.time()-start)
}
topic.coauthor.matrices[[1]]

# 
# # bind dependent measure (whether authors publish together the following year)
start <- Sys.time()
for(i in 1:(length(topicSims)-1)){
  print(paste0("year: ",years[i]))
  temp <- topic.coauthor.matrices[[i]]
  temp.next <- topic.coauthor.matrices[[i+1]] %>%
    mutate(new_publication = prior_publication) %>%
    dplyr::select(-c(prior_publication, authorsSim, year))
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











#Isabella's code to combine topic similarity and publication

# model_data = data.frame(authorA = c(),
#                         authorB = c(),
#                         new_publication = c(),
#                         prior_publication = c(),
#                         topic_similarity = c())
# 
# for(i in 1:(length(topicSims)-1)){
#   for(j in 1:nrow(topicSims[[i]])){
#     rowAuth = topicSims[[i]]$authorA[j]
#     colAuth = topicSims[[i]]$authorB[j]
#     row = which(topicSims[[i]]$authorA[j] == author_key)
#     col = which(topicSims[[i]]$authorB[j] == author_key)
#     new_pub = coauthor_mats[[i+1]][row,col]
#     prior_pub = coauthor_mats[[i]][row,col]
#     topic_sim = topicSims[[i]]$authorsSim[j]
#     model_data = bind_rows(model_data, data.frame(authorA=rowAuth, #keep track of authors, critical for removing identity "co-authors"
#                                                   authorB=colAuth,
#                                                   new_publication=new_pub,
#                                                   prior_publication=prior_pub,
#                                                   topic_similarity=topic_sim))
#   }
# }
# 
# 
# # colnames(model_data) = c("new_publication",
# #                          "prior_publication",
# #                          "topic_similarity")
# model_data
# write.csv(model_data, file = "model_data.csv")
# 
# 
# nrow(model_data)
# model_data_diffAuthors <- model_data %>%
#   filter(authorA != authorB)
# 
# head(model_data_diffAuthors)
# glm(model_data_diffAuthors, aes(new_publication ~ prior_publication + topic_similarity, family="binomial"))





# old version when topic matrix was symmetrical and co-authorship matrix was not symmetrical 
# and contained full set of author
# topic.coauthor.matrices <- list()
# years = 2000:2019
# start <- Sys.time()
# i=1
# for(i in 1:length(topicSims)){
#   print(paste0("year: ",years[i]))
#   
#   tempTop <- topicSims[[i]]
#   tempTop <- tempTop %>%
#     spread(authorB, authorsSim) %>%
#     dplyr::select(-authorA) %>%
#     as.matrix()
#   #tempTop[upper.tri(tempTop, diag = TRUE)] <- NA #ignore upper triangle
#   #peek(round(tempTop,4),15)
# 
#   tempAuth <- coauthor_mats[[i]]
#   tempAuth <- pmax.matr(tempAuth)
#   colnames(tempAuth) <- author_key
#   #peek(tempAuth, 15)
# 
#   tempTop.df <- tempTop %>%
#     as.data.frame() %>%
#     gather("authorA","topicSim",1:ncol(.)) %>%
#     mutate(authorB=rep(colnames(tempTop),length(colnames(tempTop)))) %>%
#     # mutate(authorB=rep(colnames(tempTop),length(colnames(tempTop))),
#     #        tempA = ifelse(authorA < authorB, authorA, authorB), #alphabetically first author => authorA
#     #        tempB = ifelse(authorA > authorB, authorA, authorB), #alphabetically last author => authorB
#     #        authorA = tempA, 
#     #        authorB = tempB) %>%
#     # dplyr::select(-c(tempA, tempB)) %>%
#     filter(!is.na(topicSim))
#   tempAuth.df <- tempAuth %>%
#     as.data.frame() %>%
#     gather("authorA","prior_publication",1:ncol(.)) %>%
#     mutate(authorB=rep(colnames(tempAuth),length(colnames(tempAuth)))) %>%
#     # mutate(authorB=rep(colnames(tempAuth),length(colnames(tempAuth))),
#     #        tempA = ifelse(authorA < authorB, authorA, authorB), #alphabetically first author => authorA
#     #        tempB = ifelse(authorA > authorB, authorA, authorB), #alphabetically last author => authorB
#     #        authorA = tempA, 
#     #        authorB = tempB) %>%
#     # dplyr::select(-c(tempA, tempB)) %>%
#     filter(!is.na(prior_publication))
#   tempBoth <- tempTop.df %>%
#     left_join(tempAuth.df, by=c("authorA","authorB")) #this takes the longest
# 
#   topic.coauthor.matrices[[i]] <- tempBoth %>%
#     mutate(year=years[i]) %>%
#     distinct()
#   print(Sys.time()-start)
# }