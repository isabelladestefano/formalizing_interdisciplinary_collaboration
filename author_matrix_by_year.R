library(dplyr)
library(tidyr)
source("author_network_edge_extract.R")


getAuthorMatrixByYear = function(byAuthor, journal){
  author_net <- byAuthor %>% select(title,authorAbbr,year) %>% unique()
  for(j in unique(author_net$year)){
    author_net_year = author_net %>% filter(year == j) %>% select(title, authorAbbr)
    nodes= unique(author_net_year$authorAbbr)
    author_mat_year <- matrix(rep(0,length(nodes)*length(nodes)),nrow = length(nodes), ncol = length(nodes))
    year_edges = edgeList(author_net_year)
    
    nodes=data.frame(id = 1:length(nodes), label = nodes)
    year_edges = apply(year_edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
    year_edges = data.frame(year_edges)
    mapply(function(x,y){author_mat_year[x,y]<<-1;author_mat_year[y,x]<<-1 }, year_edges$from, year_edges$to)
    names(author_mat_year) = nodes[2]
    filename = paste0("authorMatrix/",journal,"/author_mat_year_",journal,"_",j,".RData" )
    print(filename)
    save(author_mat_year, file = filename)
  }
}

#getAuthorMatrixByYear(cogsci_byAuthor, "cogsci")
#getAuthorMatrixByYear(vss_byAuthor, "vss")