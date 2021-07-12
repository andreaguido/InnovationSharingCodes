# HP 3 assortativity
## Lower connectivity is associated to higher assortment
library(psych)
library(igraph)
# upload Yule function
source("C:/Users/andrea/Dropbox/Werk - Projects/Innovation sharing/Data and Scripts/Others/Yule.R")
al <- list() # adjacency list
g <- list() # network list
sessions <- levels(as.factor(df$session.label)) # session names

# things to save plots
plots=list()
root_img <- "Data Analysis/Merged/Plots/"

final_assortment_network <- data.frame(SESSIONS=sessions, 
                                       TREATMENT = NA , 
                                       ASSORTATIVITY = NA, 
                                       YULE= NA,
                                       YULE_COOP = NA,
                                       ASSORTATIVITY_COOP = NA) # dataframe of net-lev assortatitvity

for (i in 1:length(sessions)) {
  # import session-level data
  df_temp <- df %>% subset.data.frame(,subset = df$session.label==sessions[i] & df$subsession.round_number==30, 
                                      select = c(
                                                participant.id_in_session,
                                                player.neighbors,player.share_decision,
                                                treatment
                                      ))
  
  # define graph and adjacency list
  al[[i]] <- strsplit(as.character(df_temp$player.neighbors),",")
  g[[i]] <- graph_from_adj_list(al[[i]], mode="all")
  
  # assign treatment to session
  final_assortment_network$TREATMENT[i] = levels(as.factor(df_temp$treatment))
  
  V(g[[i]])$label <- NA
  V(g[[i]])$types <- replace(df_temp$player.share_decision, df_temp$player.share_decision==0, 2)
  colrs <- c("blue", "red")
  V(g[[i]])$color <- colrs[V(g[[i]])$types]
  #jpeg(file = paste(root_img,sessions[i], '.jpeg', sep = ''))
  #plot(g[[i]], main=sessions[i])
  #dev.off()
  
  # prepare database for this network
  assortment_data <- data.frame(ID = as.vector(V(g[[i]])) , 
                                type= V(g[[i]])$types, 
                                INDIVIDUAL_ASSORTATIVITY=NA,
                                YULE = NA,
                                type_1_links=NA, 
                                type_2_links=NA)
  # compute assortativity measure (Rand et al. 2014) & Yule's)
  for (j in 1:length(df_temp$participant.id_in_session)){
    # Rand et al 2014
    # get neighbours of j
    neighbours_j = neighbors(g[[i]],j)
    neighbours_types = NA
    # get types of her neighbours
    for (k in neighbours_j){
      neighbours_types <- append(neighbours_types,assortment_data$type[k])
    }
    # save the data
    assortment_data$INDIVIDUAL_ASSORTATIVITY[j] <- sum(neighbours_types[-1]==1)/(length(neighbours_types)-1)
    
    
    # Yule
    
    # save the data
    assortment_data$type_1_links[j] <- sum(neighbours_types[-1]==1)
    assortment_data$type_2_links[j] <- sum(neighbours_types[-1]==2)
    assortment_data$YULE[j] <- compute_yule(assortment_data, j)
    
  }
    #match_type <- assortment_data %>% group_by(type) %>% summarise(sum_type_1_links = sum(type_1_links),sum_type_2_links=sum(type_2_links))
    #yule_matrix <- as.matrix(match_type[,2:3])
    #dimnames(yule_matrix) <- NULL
    #yule_q <- Yule(yule_matrix)#YuleBonett(yule_matrix,.5)$rho
  
  # pass on the data
    assortativity_by_type <- assortment_data %>% group_by(type) %>%
      summarise(mean_assortment= mean(INDIVIDUAL_ASSORTATIVITY), YULE_MEAN = mean(YULE))
  final_assortment_network$ASSORTATIVITY[i] <- assortativity_by_type$mean_assortment[1]-assortativity_by_type$mean_assortment[2]
  final_assortment_network$ASSORTATIVITY_COOP[i] <- assortativity_by_type$mean_assortment[1]
  final_assortment_network$YULE[i] <- assortativity_by_type$YULE_MEAN[1]+assortativity_by_type$YULE_MEAN[2]
  final_assortment_network$YULE_COOP[i] <- assortativity_by_type$YULE_MEAN[1]
}

# clean the dataset - remove dynamic networks

final_assortment_network <- subset.data.frame(final_assortment_network, subset = final_assortment_network$TREATMENT=="P2" | final_assortment_network$TREATMENT=="P4")
final_assortment_network %>% group_by(TREATMENT) %>% summarise(ASSORTATIVITY_RAND = mean(ASSORTATIVITY), 
                                                               YULE = mean(YULE),
                                                               ASSORTATIVITY_COOP_RAND = mean(ASSORTATIVITY_COOP),
                                                               YULE_COOP = mean(YULE_COOP))
wilcox.test(data=final_assortment_network,YULE_COOP~TREATMENT, conf.int = T)
wilcox.test(data=final_assortment_network,ASSORTATIVITY_COOP~TREATMENT, conf.int = T)
# compute p-values through permutation test
d <- final_assortment_network$YULE_COOP
n = length(d)
set.seed(706);  m = 10^5
t.obs = t.test(d)$stat;  pv.obs = t.test(d)$p.val
t.prm = replicate(m, t.test(d*sample(c(-1,1), n, rep=T))$stat)
p.val = mean(abs(t.prm) >= abs(t.obs));  p.val

cbind.data.frame(x=final_assortment_network$ASSORTATIVITY_COOP[which(final_assortment_network$TREATMENT=="P2")],
        y= final_assortment_network$ASSORTATIVITY_COOP[which(final_assortment_network$TREATMENT=="P4")])
# screenshot final round
last_round_data <- df %>% subset.data.frame(
  subset=(subsession.round_number==10 & player.bonus_flag == 1 & player.BOT_sharing==0 & player.BOT_combination==0 & (treatment=="P2"| treatment=="P4")), 
  select = c(session.label,
             treatment,
             participant.id_in_session,
             player.bonus_flag,
             player.share_decision)) %>%
  group_by(treatment) %>% 
  summarize(Perc_Coop=mean(player.share_decision))
wilcox.test(last_round_data$Perc_Coop~last_round_data$treatment)

# compute p-values through permutation test
d <- last_round_data$Perc_Coop
n = length(d)
set.seed(706);  m = 10^5
t.obs = t.test(d)$stat;  pv.obs = t.test(d)$p.val
t.prm = replicate(m, t.test(d*sample(c(-1,1), n, rep=T))$stat)
p.val = mean(abs(t.prm) >= abs(t.obs));  p.val

# evolution over rounds of 2 sessions (Session 6 and 10)
# reconstruct network
sessions_evo <-  sessions[grep("P", sessions)]#c("Session 17 P2", "Session 10 P2", "Session 11 P2", "Session 6 P4", "Session 7 P4", "Session 5 P4")
al <- list()
g <- list()
for (i in 1:length(sessions_evo)){
  color <- list()
  color[[1]] <- rep("white",15)
  df_temp <- df %>% subset.data.frame( ,subset = df$session.label==sessions_evo[i], 
                                                   select = c(
                                                     subsession.round_number,
                                                     participant.id_in_session,
                                                     player.neighbors,player.share_decision,player.bonus_flag,
                                                     treatment
                                                   ))
  types <- data.frame(round = 1:30, defectors = NA, cooperators = NA, non_innovators = NA)
  
for (t in 1:30) {
  dftempt <- df_temp %>% subset.data.frame(, subset = df_temp$subsession.round_number==t)
  edge_list <- strsplit(as.character(dftempt$player.neighbors),",")  
  al[[t]] <- edge_list
  g[[t]] <- graph_from_adj_list(al[[t]], mode="all")
  # update types
  if (t==1) {
    
  color[[t]] <- replace(color[[t]], dftempt$player.bonus_flag==1 & dftempt$player.share_decision ==1, "blue")
  color[[t]] <- replace(color[[t]], dftempt$player.bonus_flag==1 & dftempt$player.share_decision ==0, "red")
  } else {
    color[[t]] <- color[[t-1]]
    color[[t]] <- replace(color[[t]], dftempt$player.bonus_flag==1 & dftempt$player.share_decision ==1, "blue")
    color[[t]] <- replace(color[[t]], dftempt$player.bonus_flag==1 & dftempt$player.share_decision ==0, "red")
  }
  if (t==1) {
    coordinates <- layout_nicely(g[[t]])
    #jpeg(file = paste(root_img,sessions_evo[i],' Round_',t, '.jpg', sep = ''))
  #plot(g[[t]], vertex.color=color[[1]], layout=coordinates,main= paste(sessions_evo[i], "Round ", t))
  #dev.off()
  #jpeg(file = paste(root_img,'time/',sessions_evo[i],' Round_',t, '.jpg', sep = ''))
  #plot(, vertex.color=color[[1]], layout=coordinates,main= paste(sessions_evo[i], "Round ", t))
  #dev.off()
  
  } else {
    if (t==30) {
      
    jpeg(file = paste(root_img,sessions_evo[i],' Round_',t, '.jpg', sep = ''))
  plot(g[[t]], vertex.color=color[[t]], layout=coordinates,main= paste(sessions_evo[i], "Round ", t))
  dev.off()
    }
  }
# plots type over rounds
    
    n_of_coop <- sum(ifelse(as.vector(color[[t]]) == "blue", 1, 0))
    n_of_defectors <- sum(ifelse(as.vector(color[[t]]) == "red", 1, 0))
    n_of_non_innov <- sum(ifelse(as.vector(color[[t]]) == "white", 1, 0))
    types[t, -1] <- list(n_of_defectors,n_of_coop,n_of_non_innov)
    if (t==30) {
      
    jpeg(file = paste(root_img,"time/",sessions_evo[i],'.jpg', sep = ''))
    plot(types$round[1:t], types$cooperators[1:t], type="b", xlim=c(1,30), ylim=c(0,15),xlab="Round", 
         ylab="Types", col="blue", lwd=2, cex=2, pch=16, main=paste(sessions_evo[i], " Round ", t))
    lines(types$round[1:t], types$defectors[1:t], type="b", col="red", cex=2, lwd=2, pch= 16)
    lines(types$round[1:t], types$non_innovators[1:t], type="b", col="green", cex=2, lwd=2)
    dev.off()
    }
  }
  
}
