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
df <- df %>% group_by(session.code, participant.id_in_session) %>% mutate(player_unique_ID = cur_group_id())
rm(all_sessions)