setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis")
library(tidyverse)
library(plot3D)
library(pbapply)
library(stringr)
library(cluster)
library(factoextra)
topic.df <- read_csv("cogsci_topics_authorAbbr.csv") %>%
  distinct() %>%
  mutate(authors=ifelse(grepl(",",authors), gsub(",.*","",authors), authors),
         authors=ifelse(authors=="J Tenenbaums", "J Tenenbaum", authors)) %>% #manually fixing incorrect author naming
  dplyr::select(-X1)

# read in centrality
all_centrality <- data.frame()
for(i in 2000:2019){
  year_centrality <- read_csv(paste0("networkByYear/centrality_",i,".csv")) %>%
    mutate(year=i) %>%
    select(-X1)
  all_centrality <- bind_rows(all_centrality, year_centrality)
}

allAuthors <- topic.df %>%
  dplyr::select(authors) %>%
  distinct() %>%
  dplyr::arrange(authors) %>% #data frame of authors in alphabetical order
  na.omit()
### FUNCTIONS ###

# get mean topic distribution for a set of documents
get_avg_topic_dist <- function(df) {
  topic.means = df %>%
    dplyr::select(-title, -authors, -year) %>%
    colMeans()
  topic.means = as.data.frame(topic.means)
  return(topic.means)
}

global_average = get_avg_topic_dist(topic.df)

selectTopicByAuthor <- function(author, df=topic.df){
  df %>%
    filter(authors == author) %>%
    get_avg_topic_dist()
}

selectSmoothTopicByAuthor <- function(author, n, df=topic.df){
  df %>%
    filter(authors == author) %>%
    bind_rows(as.data.frame(matrix(c(rep(NA,3), global_average$topic.means), #binds fake rows of global (smoothing)
                                   ncol=103, nrow=n, byrow=TRUE, dimnames = list(NULL, names(topic.df))))) %>%
    get_avg_topic_dist()
}

#################
selectTopicByAuthor("E Vul", topic.df)
selectSmoothTopicByAuthor("E Vul", 2, topic.df)
selectSmoothTopicByAuthor("E Vul", 2, years.df)

(len.authors = nrow(allAuthors))
authorTopics.df <- allAuthors %>%
  mutate(authorTopics = pbmapply(selectSmoothTopicByAuthor, authors, MoreArgs=list(n=2))) %>%
  unnest(authorTopics) %>%
  mutate(topic = paste0("topic",str_pad(rep(1:100, len.authors), 3, pad = "0"))) %>%
  spread(topic, authorTopics) %>%
  na.omit()

d <- dist(authorTopics.df[,2:101])
fit <- cmdscale(d, eig=TRUE, k=2)
fitdf <- fit$points %>%
  as.data.frame() %>%
  bind_cols(allAuthors)
write_csv(fitdf, "fitMDS2.csv")

ggplot(fitdf, aes(x=V1, y=V2, label=authors)) +
  geom_text(alpha=0.5, size=2)
ggsave("img/MDS_2_smooth.png")

fit3 <- cmdscale(d, eig=TRUE, k=3)
fitdf3 <- fit3$points %>%
  as.data.frame() %>%
  bind_cols(allAuthors)
write_csv(fitdf3, "fitMDS3.csv")

plot_ly(fitdf3, x=~V1, y=~V2, z=~V3, size=0.2, alpha=0.2)


glimpse(fitdf)



start <- Sys.time()
authorClusters = list()
for(i in 1:10){
  temp.authorCluster <- kmeans(fitdf[, 1:2], i, nstart = 25)
  authorClusters[[i]] = temp.authorCluster
  print(Sys.time()-start)
}

fitdf.final <- fitdf[,1:2]
rownames(fitdf.final) <- fitdf$authors

for(i in 1:10){
  fviz_cluster(authorClusters[[i]], data = fitdf.final, labelsize=5)
  ggsave(paste0("img/kmeans/full_kmeans_",i,".png"))
}

evalK = data.frame()
for(i in 1:10){
  evalK <- bind_rows(evalK,
                     data.frame(k=i,
                                betweenss=authorClusters[[i]]$betweenss,
                                tot.withinss=authorClusters[[i]]$tot.withinss,
                                totss=authorClusters[[i]]$totss))
}
evalK %>%
  mutate(b_totss = betweenss/totss) %>%
  dplyr::arrange(desc(b_totss)) %>%
  ggplot(aes(x=k, y=tot.withinss)) +
  geom_line() +
  geom_point() +
  scale_y_continuous("total within SS")
ggsave("img/kmeans/total_within.png")






################

#### MDS by Year ####
years = 2000:2019
mds.year = list()
for(i in 1:20){
  years.df <- topic.df %>%
    filter(year == years[i])
  
  authors.year <- years.df %>%
    dplyr::select(authors) %>%
    distinct() %>%
    dplyr::arrange(authors) %>%
    na.omit()
  
  authorsTopics.df.year <- authors.year %>%
    mutate(authorTopics = pbmapply(selectSmoothTopicByAuthor, authors, MoreArgs=list(n=2, df=years.df))) %>%
    unnest(authorTopics) %>%
    mutate(topic = paste0("topic",str_pad(rep(1:100, length(unique(authors.year$authors))), 3, pad = "0"))) %>%
    spread(topic, authorTopics) %>%
    na.omit()
  
  d <- dist(authorsTopics.df.year[,2:101])
  fit <- cmdscale(d, eig=TRUE, k=2)
  fitdf <- fit$points %>%
    as.data.frame() %>%
    bind_cols(authors.year)
  mds.year[[i]] = fitdf %>%
    mutate(year=years[i])
  write_csv(fitdf, paste0("mdsfits/mdsfit_",years[i],".csv"))
  
  central_auth = all_centrality %>%
    filter(year == years[i], CM == "eigen") %>%
    top_n(1, measure) %>%
    .$label
  ggplot(fitdf, aes(x=V1, y=V2, label=authors)) +
    geom_text(alpha=0.5, size=2) +
    geom_text(data=filter(fitdf, authors==central_auth), colour="red", alpha=0.5, size=2)
  ggsave(paste0("img/mds_byYear/mds_nokmeans/mds_",years[i],".png"))
}


############################################

#### k Means by Year ####
mds.year = list()
for(i in 1:length(years)){
  temp <- read_csv(paste0("mdsfits/mdsfit_",years[i],".csv"))
  mds.year[[i]] = temp
}

for(i in 1:length(mds.year)){
  fitdf.final <- mds.year[[i]][,1:2]
  rownames(fitdf.final) <- mds.year[[i]]$authors
  
  start <- Sys.time()
  authorClusters = list()
  for(j in 1:10){
    authorClusters[[j]] <- kmeans(fitdf.final, j, nstart = 25)
    fviz_cluster(authorClusters[[j]], data = fitdf.final, labelsize=5)
    ggsave(paste0("img/mds_byYear/kmeans_",years[i],"/",years[i],"_kmeans_",j,".png"))
    print(Sys.time()-start)
  }
  
  evalK = data.frame()
  for(j in 1:10){
    evalK <- bind_rows(evalK,
                       data.frame(k=j,
                                  tot.withinss=authorClusters[[j]]$tot.withinss))
  }
  ggplot(evalK, aes(x=k, y=tot.withinss)) +
    geom_line() +
    geom_point() +
    scale_y_continuous("total within SS")
  ggsave(paste0("img/mds_byYear/kmeans_",years[i],"/total_within.png"))
}


#### compare k=5 across years ####

mds.year.5 = list()
mds.year.ss = data.frame()
years=2000:2019
for(i in 1:length(mds.year)){
  fitdf.final <- mds.year[[i]][,1:2]
  rownames(fitdf.final) <- mds.year[[i]]$authors
  
  for(j in 5:10){
    mds.year.5[[i]] <- kmeans(fitdf.final, j, nstart = 25)
    mds.year.ss <- bind_rows(mds.year.ss,
                             data.frame(year=years[i],
                                        k=j,
                                        tot.withinss=mds.year.5[[i]]$tot.withinss,
                                        betweenss=mds.year.5[[i]]$betweenss,
                                        totss=mds.year.5[[i]]$totss))
  }
}

mds.year.ss %>%
  filter(k==5) %>%
  mutate(within_bw = tot.withinss/betweenss) %>%
  ggplot(aes(x=year, y=within_bw)) +
  geom_line() +
  scale_y_continuous("total within / between SS for k=5")



# mds.year.ss %>%
#   filter(k==5) %>%
#   mutate(within = tot.withinss) %>%
#   ggplot(aes(x=year, y=within)) +
#   geom_line()






#### MDS for Specific Year ####
# years.df <- topic.df %>%
#   filter(year == 2019)
# 
# authors.2019 <- years.df %>%
#   dplyr::select(authors) %>%
#   distinct() %>%
#   dplyr::arrange(authors)
# 
# authorsTopics.df2019 <- authors.2019 %>%
#   mutate(authorTopics = pbmapply(selectSmoothTopicByAuthor, authors, MoreArgs=list(n=2, df=years.df))) %>%
#   unnest(authorTopics) %>%
#   mutate(topic = paste0("topic",str_pad(rep(1:100, length(unique(years.df$authors))), 3, pad = "0"))) %>%
#   spread(topic, authorTopics) %>%
#   na.omit()
# 
# # Classical MDS
# d <- dist(authorsTopics.df2019[,2:101])
# 
# ## 2 dimensions
# fit <- cmdscale(d, eig=TRUE, k=2)
# fitdf <- fit$points %>%
#   as.data.frame() %>%
#   bind_cols(authors.2019)
# 
# ggplot(fitdf, aes(x=V1, y=V2, label=authors)) +
#   geom_text(alpha=0.5, size=2)
# 
# ## 3 dimensions
# fit <- cmdscale(d, eig=TRUE, k=3)
# fitdf <- fit$points %>%
#   as.data.frame() %>%
#   bind_cols(authors.2019)
# plot_ly(fitdf, x=~V1, y=~V2, z=~V3, size=0.2, alpha=0.2) #plot in 3D
# 
# 
# #### isoMDS() ####
# library(MASS)
# # d <- dist(t(authorsTopics.df2019[,2:101]))
# # fit <- isoMDS(d, k=2)
# # fitdf <- fit$points %>%
# #   as.data.frame() %>%
# #   bind_cols(topics=rownames(fit$points))
# # 
# # ggplot(fitdf, aes(x=V1, y=V2, label=topics)) +
# #   geom_text(alpha=0.5, size=2)
# 
# 
# distinctAuthTops <- authorsTopics.df2019[2:101] %>%
#   distinct() # need to remove "duplicate authors", i.e. same topic, to do isoMDS
# d <- dist(distinctAuthTops)
# fit <- isoMDS(d, k=2)
# fitdf <- fit$points %>%
#   as.data.frame()
# ggplot(fitdf, aes(x=V1, y=V2)) +
#   geom_point()
# 
# fit <- isoMDS(d, k=3)
# fitdf <- fit$points %>%
#   as.data.frame()
# plot_ly(fitdf, x=~V1, y=~V2, z=~V3, size=0.2, alpha=0.2)
# 
# 
# 
# start <- Sys.time()
# set.seed(1000)
# authorClusters = list()
# for(i in 1:30){
#   temp.authorCluster <- kmeans(authorTopics.df[, 2:101], i, nstart = 100)
#   authorClusters[[i]] = temp.authorCluster
#   print(Sys.time()-start)
# }
# 
# authorClusters[[30]]
# evalK = data.frame()
# for(i in 1:30){
#   evalK <- bind_rows(evalK,
#                         data.frame(k=i,
#                                    betweenss=authorClusters[[i]]$betweenss,
#                                    tot.withinss=authorClusters[[i]]$tot.withinss,
#                                    totss=authorClusters[[i]]$totss))
# }
# evalK %>%
#   mutate(b_totss = betweenss/totss) %>%
#   dplyr::arrange(desc(b_totss))
# 
# 
# 
