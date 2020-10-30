#This deals with centrality and is not analysis relevant to the paper

require(cleanNLP)
require(udpipe)
require(stringi)
library(knitr)
library(igraph)

source('author_network_edge_extract.R')

writeAuthorNet = function(author_net){
  all_nodes = unique(author_net$authorAbbr) 
  all_edges = edgeList(author_net%>%select(title,authorAbbr)) #do not run this line with full author_net set it will take HOURS load csv 'all_edges.csv' instead
  
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

  all_centrality = data.frame(id = all_nodes$id, 
                              label = all_nodes$label, 
                              measure = degree_centrality$res, 
                              CM = rep('degree', length(all_nodes$id)))
  
  return(all_centrality)
}

centralityQuantile = function(dat, q = c(.95,1), cent='degree'){
  q1=q[1]
  q2=q[2]
  
  dat = dat %>% 
    filter(CM == cent) %>%
    arrange(desc(measure)) 
  
  nAuthors = length(dat$label)
  
  dat %>% 
    mutate(index = 1:nAuthors, pct = (1 - index/nAuthors)) %>%
    filter(q1<=pct, pct<q2)%>%
    select(label,CM, measure,pct) 
}

getAuthorList = function(df){
  editAuthors <- function(origAuthors){
    temp <- matrix(strsplit(origAuthors, ",")[[1]], , 2, byrow=TRUE)
    author <- paste(trimws(temp[,2]), trimws(temp[,1]), sep=" ")
    return(author)
  }
  
  byAuthor1 <- df%>%
    filter(year <= 2014 & year >= 2000) %>%
    mutate(author = mapply(editAuthors, authors)) %>%
    unnest(author) %>%
    mutate(authorAbbr = case_when(
      word(author, -1) %in% c("II", "III", "IV", "Jr.", "Jr", "Ph.D.", "PhD") ~ paste(substring(author,1,1), word(author, -2)),
      word(author, 1) %in% c("Dr.", "Dr") ~ paste(substring(word(author,2),1,1), word(author, -1)),
      TRUE ~ paste(substring(author,1,1), word(author, -1))
    ))
  
  
  byAuthor2 <- df %>%
    filter(year > 2014 | year < 2000) %>%
    mutate(authors=stri_trans_general(authors, "latin-ascii"),
           author=str_replace_all(authors, ",$", ""),
           author=str_replace_all(author, c(".*\n"="", " *\\(.*?\\)"="", "  "=" ")),
           author=str_replace_all(author, " & ", ", "),
           author=strsplit(author, ", ")) %>%
    unnest(author) %>%
    filter(!grepl("University|Institute|Center|Centre|Centro|School|Department|Dept|Unit|Hospital|\\d",
                  author)) %>%
    mutate(author=trimws(author),
           authorAbbr = case_when(
             word(author, -1) %in% c("II", "III", "IV", "Jr.", "Jr", "Ph.D.", "PhD") ~ paste(substring(author,1,1), word(author, -2)),
             word(author, 1) %in% c("Dr.", "Dr") ~ paste(substring(word(author,2),1,1), word(author, -1)),
             TRUE ~ paste(substring(author,1,1), word(author, -1))
           ))
  
  byAuthor <- bind_rows(byAuthor1, byAuthor2)
}

transform_authorAbbr = function(df){
  newAuthors <- getAuthorList(df) %>%
    group_by(title) %>%
    nest(authorAbbr) %>%
    mutate(authors = map(data, unlist),
           authors = map_chr(authors, paste, collapse = ", ")) %>%
    select(title, authors)
  newdf <- left_join(df, newAuthors, by="title") %>%
    mutate(authors = authors.y) %>%
    select(-c(authors.x,authors.y)) %>%
    select(title, authors, everything())
  return(newdf)
}

