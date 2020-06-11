setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/vss_analysis")
library(shiny)
library(tidyverse)
library(visNetwork)
library(colorspace)
library(igraph)

whichYear = "2019" #all for all years

ui <- fluidPage(
  textInput('textname', label = 'Author:', placeholder = 'E Vul'),
  selectInput('CM', label = 'Centrality Measure:', choices = c('degree','close','between','eigen'), selected = c('degree')),
  visNetworkOutput("author_network", height = "1000px")
  
)
input = ui
output = server

server<- function(input,output){

  nodes<- read.csv(paste0('centrality_byYear/nodes_',whichYear,'.csv')) %>%
    select(-X)
  edges<- read.csv(paste0('centrality_byYear/edges_',whichYear,'.csv'))
  edges$X = NULL
  
  edges = data.frame(edges)
  edges = apply(edges, MARGIN = c(1,2), function(x){return(which(nodes[2] == x))})
  nodes = data.frame(nodes) 
  names(nodes) = c('id','label')
  edges = data.frame(edges)
  
  centrality  = read.csv(paste0('centrality_byYear/centrality_',whichYear,'.csv'))
  

  output$author_network <- renderVisNetwork({
    nodes = data.frame(centrality) %>% 
      filter(label %in% nodes[,2], CM == input$CM) %>% 
      select(label,measure)%>% 
      inner_join(nodes) %>% 
      arrange(desc(measure)) %>% 
      select(id,label)
    
    nodes$color = sequential_hcl(length(nodes$id))
    
    visNetwork(nodes, edges, width = "150%")
    
  })
  
  
  observe({
    if(input$textname %in% nodes$label){
    focus = which(nodes$label == as.character(input$textname))
    
    visNetworkProxy("author_network") %>%
      visFocus(id = focus , scale = 4, locked = F, offset = list(x=0,y=10))
    }
  })
}

shinyApp(ui = ui, server = server)


