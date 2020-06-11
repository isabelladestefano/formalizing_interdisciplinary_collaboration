#################################################
##### Compare k means across VSS and CogSci #####
#################################################

setwd("/Users/loey/Desktop/Research/InfluencingCogSci/R/cogsci_analysis/compare_CogSci_VSS/kmeans/")
library(tidyverse)
library(factoextra)
library(RColorBrewer)
my_orange = brewer.pal(n = 9, "YlOrBr")[3:7]
my_blue = brewer.pal(n = 9, "PuBuGn")[5:9]
my_conf_colours = c(
  "CogSci" = "#027880",
  "VSS" = "#e0772f"
)

#### load k means by year ####
mds.year = list()
cogsci_years=2000:2019
for(i in 1:length(cogsci_years)){
  temp <- read_csv(paste0("cogsci_mdsfits/mdsfit_",cogsci_years[i],".csv")) %>%
    mutate(conference="CogSci",
           year=cogsci_years[i])
  mds.year[[i]] = temp
}
vss_years=2001:2019
for(i in 1:length(vss_years)){
  temp <- read_csv(paste0("vss_mdsfits/mdsfit_",vss_years[i],".csv")) %>%
    mutate(conference="VSS",
           year=vss_years[i])
  mds.year[[i+length(cogsci_years)]] = temp
}

for(n in 1:length(mds.year)){
  print(nrow(mds.year[[n]]))
}

#### compare k=5 across years ####
set.seed(123)
kmean.5 = list()
kmean.5.fig = list()
kmean.ss = data.frame()
for(i in 1:length(mds.year)){
  fitdf.final <- mds.year[[i]][,1:2]
  rownames(fitdf.final) <- mds.year[[i]]$authors
  conf = unique(mds.year[[i]]$conference)
  yr = unique(mds.year[[i]]$year)
  
  kmean.5[[i]] <- kmeans(fitdf.final, 5, nstart = 25)
  if(conf=="CogSci"){
    kmean.5.fig[[i]] <- fviz_cluster(kmean.5[[i]], data = fitdf.final, labelsize=5) +
      ggtitle("") +
      scale_colour_manual(values=my_blue) +
      scale_fill_manual(values=my_blue) +
      theme_minimal() +
      theme(legend.position="none")
  } else{
    kmean.5.fig[[i]] <- fviz_cluster(kmean.5[[i]], data = fitdf.final, labelsize=5) +
      ggtitle("") +
      scale_colour_manual(values=my_orange) +
      scale_fill_manual(values=my_orange) +
      theme_minimal() +
      theme(legend.position="none")
  }
  
  ggsave(paste0("img/cluster_",conf,"_",yr,".png"),kmean.5.fig[[i]])
  kmean.ss <- bind_rows(kmean.ss,
                        data.frame(conference=conf,
                                   year=yr,
                                   k=5,
                                   tot.withinss=kmean.5[[i]]$tot.withinss,
                                   betweenss=kmean.5[[i]]$betweenss,
                                   totss=kmean.5[[i]]$totss))
}


# analyzing fit
## aim to minimize total within sum of squares (within group similarity)
## aim to maximize between sum of squares (between group dissimilarity)
kmean.ss %>%
  mutate(within_bw = tot.withinss/betweenss) %>%
  ggplot(aes(x=year, y=within_bw, colour=conference)) +
  geom_line() +
  #scale_x_continuous(limits=c(2000,2020)) +
  #scale_y_continuous("total within / between SS for k=5", limits=c(0,0.5)) +
  facet_wrap(~k) +
  theme_minimal()


kmean.ss %>%
  filter(k == 5) %>%
  mutate(within_bw = tot.withinss/betweenss) %>%
  ggplot(aes(x=year, y=within_bw, colour=conference)) +
  geom_line(size=2) +
  scale_x_continuous("Year", limits=c(2000,2020)) +
  scale_y_continuous("Total Within / Between SS", limits=c(0,0.5)) +
  scale_colour_manual(values=my_conf_colours) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = .4))
ggsave("img/kmeans_fit.png", width=9, height=4)

kmeans.model.df <- kmean.ss %>%
  filter(k == 5) %>%
  mutate(within_bw = tot.withinss/betweenss)

kmeans.m <- lm(within_bw ~ conference+year, data=kmeans.model.df)
summary(kmeans.m)

kmean.model <- kmean.ss %>%
  filter(k == 5) %>%
  mutate(within_bw = tot.withinss/betweenss)

model <- lm(within_bw ~ year, data=filter(kmean.model, conference=="CogSci"))
summary(model)

model <- lm(within_bw ~ year, data=filter(kmean.model, conference=="VSS"))
summary(model)









