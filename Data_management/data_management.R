# read file csv
## read files names
sessions_files <- list.files("Data Innovation sharing/game/")
read_files <- function(x) read.csv(x,stringsAsFactors = F)
## import into a list
all_sessions <- list.files("Data Innovation sharing/game/", pattern = "*.csv", full.names = T) %>% lapply(read_csv) %>% bind_rows()
# drop demos
all_sessions <- all_sessions[which(all_sessions$session.is_demo!=1 & all_sessions$subsession.number_of_nodes == 15),]
# select columns
c <- read.table("Data_management/columns_selected.txt", sep = ",", header = T)
c.num <- sapply(colnames(c), function(i){
  which(colnames(all_sessions)==i)
})
df <- all_sessions[,unlist(c.num, use.names = F)]

# define treatment variable
df$treatment <- paste(df$subsession.matching, df$subsession.degree, sep="")
# define unique id for each player
df <- df %>% mutate(player_unique_ID = group_indices(., session.code, participant.id_in_session))
rm(all_sessions)

## DEPRECATED CHUNCKS ##
#import.list <- llply(paste("Data Innovation sharing/game/",sessions_files, sep=""),read_files)
## unlist files
#all_sessions2 <- Reduce(function(x,y) rbind.data.frame(x,y), import.list, accumulate=F)
