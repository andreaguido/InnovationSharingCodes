# this function allows to check the network structure

library(igraph)
setwd("C:/Users/andrea/Dropbox/Werk - Projects/Innovation sharing/Data and Scripts/Data Innovation sharing/game/")

rm(list=ls())
# read file csv
d <- read.csv("sessions_1_game.csv", header = T, sep = ",")
# drop demos
d <- d[which(d$session.is_demo!=1),]
c <- as.matrix(read.table("../../Data_management/columns_selected.txt", sep = ",", header = T))
c.num <- sapply(colnames(c), function(i){
    which(colnames(d)==i)
  })
df <- d[,unlist(c.num, use.names = F)]

# lists
al <- list() # adjacency list
g <- list()

# select session
#df <- subset(df, subset = df$session.label == "Session 1 - PK2 Session 1 - SK2")
# reconstruct network
for (t in 1:max(df$subsession.round_number)) {
  dftemp <- df[which(df$subsession.round_number==t),]
  edge_list <- strsplit(as.character(dftemp$player.neighbors),",")  
  al[[t]] <- edge_list
  g[[t]] <- graph_from_adj_list(al[[t]], mode="all")
  plot(g[[t]], main= paste("Round ", t))
}
