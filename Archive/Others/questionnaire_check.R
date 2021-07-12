# analysis of questionnaires

## upload data from old questionnaires
## notes: some files have ";" as sep. I upload them separately then merge em all
questionnaires_files <- list.files("Data Analysis/Merged/Questionnaires/")
questionnaires_files_1 <- questionnaires_files[-c(which(questionnaires_files=="Questionnaire_5_du_13.csv"),which(questionnaires_files=="Questionnaire_6_7_8_du_13.csv"))]
questionnaires_files_2 <- questionnaires_files[c(which(questionnaires_files=="Questionnaire_5_du_13.csv"),which(questionnaires_files=="Questionnaire_6_7_8_du_13.csv"))]
c <- read.table("Analysis Scripts/Data_management/questionnaire_columns_selected.txt", sep = ",", header = T)

read_files <- function(x, s=","){
r <- read.csv(x,stringsAsFactors = F, sep = s)
c.num <- sapply(colnames(c), function(i){
  which(colnames(r)==i)
})
r <- r[which(r$session.is_demo!=1),]
print("print")
r[,unlist(c.num, use.names = F)]
} 
import.list <- llply(paste("Data Analysis/Merged/Questionnaires/",questionnaires_files_1, sep=""),read_files)
data_questionnaire <- Reduce(function(x,y) rbind.data.frame(x,y), import.list, accumulate=F)

import.list_2 <- llply(paste("Data Analysis/Merged/Questionnaires/",questionnaires_files_2, sep=""),read_files, s=";")
data_questionnaire_2 <- Reduce(function(x,y) rbind.data.frame(x,y), import.list_2, accumulate=F)

data_questionnaire <- rbind.data.frame(data_questionnaire,data_questionnaire_2)
# remove BOTs and unfilled questionnaires
dq <- data_questionnaire %>% subset.data.frame(subset = (is.na(participant.label) == FALSE))
# add treatment variables
dq$K <- 2
dq$K <- replace(dq$K,list = grep("S4", dq$session.label),4)
dq$K <-replace(dq$K,list = grep("P4", dq$session.label),4)
dq$P <- 0
dq$P <-replace(dq$P,list = grep("S4", dq$session.label),1)
dq$P <-replace(dq$P,list = grep("S2", dq$session.label),1)
dq$right_P <- dq$player.check_matching == dq$P
dq$right_K <- dq$player.check_partners == dq$K
dq$treatment <- "P2"
dq$treatment <- replace(dq$treatment,list = grep("S2", dq$session.label),"S2")
dq$treatment <- replace(dq$treatment,list = grep("S4", dq$session.label),"S4")
dq$treatment <- replace(dq$treatment,list = grep("P4", dq$session.label),"P4")
##########
summary(dq$player.sex)
summary(dq$player.student)
summary(dq$right_P)
summary(dq$right_K)
# get IDs of subjects who got check questions right
IDs_right <- dq %>% subset.data.frame(subset = right_P==1 & right_K==1, select = participant.label)
