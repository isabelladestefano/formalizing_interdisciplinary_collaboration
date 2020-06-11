setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
library(tidyverse)
byAuthor <- read_csv("vss_byAuthor.csv")
#Need to run Lauren's script before running this one.

#helper functions
getEdges = function(x){
  from = c()
  to = c()
  for(i in 1:length(x)){
    for(j in 1:length(x)){
      if(i!=j && i>j){
        from=c(from, x[i])
        to = c(to,x[j])
      }
    }
  }
  return(data.frame(from=from,to=to))
}

edgeList = function(author_net,uniqueEdges = T){
  edges = data.frame(from = c(),to =c())
  for(k in unique(author_net$title)){
    temp = author_net %>% filter(title == k)
    if(length(temp$authorAbbr) == 1){
      edges_itt = data.frame(from = temp$authorAbbr,to = temp$authorAbbr)
    }
    else{
      edges_itt = getEdges(temp$authorAbbr)
    }
    edges = rbind(edges,edges_itt)
    print(paste0(which(unique(author_net$title) == k), ' out of ', length(unique(author_net$title))))
  }
  if(uniqueEdges && length(edges!=0)){
    edges = edges[!duplicated(apply(edges,1,function(x) paste(sort(x),collapse=''))),]
  }
  return(edges)
}


##Author network data
#Some authors are not connected to the main part of the network. This happens when they have one publication/other reasons. This is bad for measuring certain kinds of centrality because they depend on paths and loops through the network. Taking the most published authors is expedient, but a more sophisticated means should be considered.  Additionally our graph is not simple because there are multiple connections between authors, if we select unique connections the network will be more amenable (including the removeDuplicates function in getEdges will do this). 

#includes all authors
author_net = byAuthor %>% dplyr::select(title,authorAbbr,year) %>% unique()
author_net
  

#removes authors with <=n publications
# n.pubs=5
# author_net = author_net %>% 
#   group_by(authorAbbr) %>% 
#   summarise(count = n()) %>% 
#   filter(count<=n.pubs) %>% 
#   anti_join(author_net,.)

#histogram of number of publications
# author_net %>% 
#   group_by(authorAbbr) %>% 
#   summarise(count = n()) %>% ungroup() %>% 
#   ggplot(aes(x=reorder(authorAbbr,count),y=count)) + geom_boxplot(stat = 'identity')

##Author Network
#nodes and edges

#Creating an edge list to be used with igraph (for calculating centrality) and visNetwork (fun visualization compatable with shiny for writing interactive app).

#visNetwork version of nodes and edges
all_nodes = unique(author_net$authorAbbr) 
all_edges = edgeList(author_net%>%dplyr::select(title,authorAbbr)) #do not run this line with full author_net set it will take HOURS load csv 'all_edges.csv' instead
length(all_nodes)
write.csv(all_nodes, "vss_networkByYear/nodes_all.csv")
nrow(all_edges)
write.csv(all_edges, "vss_networkByYear/edges_all.csv")
library(igraph)

for(j in unique(author_net$year)){
  author_net_year = author_net %>% filter(year == j) %>% dplyr::select(title, authorAbbr)
  year_nodes= unique(author_net_year$authorAbbr)
  year_edges = edgeList(author_net_year)
  write.csv(year_edges, paste0('vss_networkByYear/edges_',j,'.csv'))
  
  year_nodes=data.frame(id = 1:length(year_nodes), label = year_nodes)
  write.csv(year_nodes, paste0('vss_networkByYear/nodes_',j,'.csv'))
  
  year_edges = apply(year_edges, MARGIN = c(1,2), function(x){return(which(year_nodes[2] == x))})
  year_edges = data.frame(year_edges)
  
  
  graph_edges = c()
  
  for(i in 1:dim(year_edges)[1]){
    graph_edges = c(graph_edges, year_edges[i,])
  }
  graph_edges = as.numeric(graph_edges)
  graph = make_graph(graph_edges, directed = FALSE)
  
  
  #We can certainly use degree centrality, but need to carefully consider other measures because of the weird structure of the graph. igraph has other useful network analysis functions that might help us systematically trim the graph to a simple and fully connected one.
  #centrality (degree, close, between)
  degree_centrality = centr_degree(graph)
  closeness_centrality = centr_clo(graph)
  betweenness_centrality = centr_betw(graph)
  eigencentrality = eigen_centrality(graph)
  
  centrality = data.frame(id = rep(year_nodes$id,4), 
                          label = rep(year_nodes$label,4), 
                          measure = c(degree_centrality$res, closeness_centrality$res, betweenness_centrality$res, eigencentrality$vector), 
                          CM = c(rep('degree', length(year_nodes$id)), 
                                 rep('close', length(year_nodes$id)), 
                                 rep('between', length(year_nodes$id)),
                                 rep('eigen', length(year_nodes$id))))
  
  write.csv(centrality, paste0('vss_networkByYear/centrality_',j,'.csv'))
  
}


#visNetwork interactive graph
#This can be used with shiny. Interactive properties are cool. 
#For example can add entry of author and have the app focus on that node. 
#Can also add coloring to the graph based on centrality measures. 

all_nodes=data.frame(id = 1:length(all_nodes), label = all_nodes)

all_edges = apply(all_edges, MARGIN = c(1,2), function(x){return(which(all_nodes[2] == x))})
all_edges = data.frame(all_edges)


#igraph
#igraph has some functions for calculating centrality on network objects(?). Object for igraph created below.  

graph_edges = c()

for(i in 1:dim(all_edges)[1]){
  graph_edges = c(graph_edges, all_edges[i,])
}
graph_edges = as.numeric(graph_edges)
graph = make_graph(graph_edges, directed = FALSE)


#We can certainly use degree centrality, but need to carefully consider other measures because of the weird structure of the graph. igraph has other useful network analysis functions that might help us systematically trim the graph to a simple and fully connected one.
#centrality (degree, close, between)
degree_centrality = centr_degree(graph)
closeness_centrality = centr_clo(graph)
betweenness_centrality = centr_betw(graph)
eigencentrality = eigen_centrality(graph)

all_centrality = data.frame(id = rep(all_nodes$id,4), 
                        label = rep(all_nodes$label,4), 
                        measure = c(degree_centrality$res, closeness_centrality$res, betweenness_centrality$res, eigencentrality$vector), 
                        CM = c(rep('degree', length(all_nodes$id)), 
                               rep('close', length(all_nodes$id)), 
                               rep('between', length(all_nodes$id)),
                               rep('eigen', length(all_nodes$id))))
write.csv(all_centrality, 'vss_networkByYear/centrality_all.csv')

#Space I was using to explore centrality.

byEigen <- centrality %>% 
  group_by(CM) %>% 
  top_n(50,measure) %>% 
  select(label,CM, measure) %>%
  #mutate(position=1:20) %>%
  ungroup() %>%
  group_by(label) %>%
  spread(CM, measure) %>%
  arrange(desc(eigen), desc(between))
View(byEigen)


library(gridExtra)
library(grid)

grid.table(byEigen)


wide_centrality = centrality%>% spread(CM,measure)

#order nodes by centrality measure
CM_test = c('degree','close','between')
nodes = data.frame(label = centrality %>% filter(CM == CM_test[1]) %>% arrange(measure) %>% pull(label))

