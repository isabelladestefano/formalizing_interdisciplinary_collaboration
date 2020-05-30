#helper functions to get network from a author_net (title, authorAbbr, year) data structure
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