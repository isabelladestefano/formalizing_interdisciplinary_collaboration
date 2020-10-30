#this is a shiny app that generates visualization of the co-authorship network.

library(shiny)
library(tidyverse)
library(visNetwork)
library(colorspace)
library(igraph)

#whichYear = "2000" #all for all years

ui <- fluidPage(
  textInput('textname', label = 'Author:', placeholder = 'E Vul'),
  selectInput('CM', label = 'Centrality Measure:', choices = c('degree','close','between','eigen'), selected = c('degree')),
  visNetworkOutput("cogsci_analysis/cogsci_networkByYear/author_network2000", height = "500px"),
  visNetworkOutput("cogsci_analysis/cogsci_networkByYear/author_network2019", height = "1000px")
)



server<- function(input,output){
  whichYear = 2000
  nodes <- read.csv(paste0('cogsci_analysis/cogsci_networkByYear/nodes_',whichYear,'.csv')) %>%
    dplyr::select(-X)
  edges <- read.csv(paste0('cogsci_analysis/cogsci_networkByYear/edges_',whichYear,'.csv'))
  edges$X = NULL
  
  edges = data.frame(edges)
  edges = apply(edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
  nodes = data.frame(nodes) 
  names(nodes) = c('id','label')
  edges = data.frame(edges)
  
  centrality  = read.csv(paste0('cogsci_analysis/cogsci_networkByYear/centrality_',whichYear,'.csv'))
  

  output$author_network2000 <- renderVisNetwork({
    nodes = data.frame(centrality) %>% filter(label %in% nodes[,2], CM == input$CM) %>% select(label,measure)%>% inner_join(nodes) %>% arrange(desc(measure)) %>% select(id,label)
    
    nodes$color = sequential_hcl(length(nodes$id))
    
    visNetwork(nodes, edges, width = "150%")
    
  })
  
  
  whichYear2 = 2019
  nodes <- read.csv(paste0('cogsci_analysis/cogsci_networkByYear/nodes_',whichYear2,'.csv')) %>%
    dplyr::select(-X)
  edges <- read.csv(paste0('cogsci_analysis/cogsci_networkByYear/edges_',whichYear2,'.csv'))
  edges$X = NULL
  
  edges = data.frame(edges)
  edges = apply(edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
  nodes = data.frame(nodes) 
  names(nodes) = c('id','label')
  edges = data.frame(edges)
  
  centrality  = read.csv(paste0('cogsci_analysis/cogsci_networkByYear/centrality_',whichYear2,'.csv'))
  
  
  output$author_network2019 <- renderVisNetwork({
    nodes = data.frame(centrality) %>% filter(label %in% nodes[,2], CM == input$CM) %>% select(label,measure)%>% inner_join(nodes) %>% arrange(desc(measure)) %>% select(id,label)
    
    nodes$color = sequential_hcl(length(nodes$id))
    
    visNetwork(nodes, edges, width = "150%")
    
  })
  
  
  
  
  
  observe({
    if(input$textname %in% nodes$label){
    focus = which(nodes$label == as.character(input$textname))
    
    visNetworkProxy("cogsci_analysis/cogsci_networkByYear/author_network2000") %>%
      visFocus(id = focus , scale = 4, locked = F, offset = list(x=0,y=10))
    visNetworkProxy("cogsci_analysis/cogsci_networkByYear/author_network2019") %>%
      visFocus(id = focus , scale = 4, locked = F, offset = list(x=0,y=10))
    }
  })
}

shinyApp(ui = ui, server = server)


