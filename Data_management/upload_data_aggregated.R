## Individual-Aggregated data ------ 1 obs per player (aggregated over rounds)
## data used for HP1 and HP2

# bonus share is the clean df including only sharing data (excluding missed decisions) --------------------------
bonus_share <- df %>% subset.data.frame( subset = (player.bonus_flag==1 & player.BOT_sharing==0),
                                         select=c(session.code,session.label,
                                                  treatment,
                                                  subsession.round_number,
                                                  participant.id_in_session,
                                                  player.share_decision,
                                                  player.bonus_flag,
                                                  player.accrued_bonuses,
                                                  player_unique_ID)) %>%
  group_by(treatment, session.code, session.label, player_unique_ID) %>% 
  summarize(perc_share=mean(player.share_decision), 
            n_sharing= sum(player.share_decision),
            n_bonuses = length(player.bonus_flag), 
            n_bonuses_received = max(player.accrued_bonuses) - n_bonuses)
# add variables to data
bonus_share$k <- 0
bonus_share$matching <- 0
bonus_share$id_matching <- 2
bonus_share$k <- replace(bonus_share$k, bonus_share$treatment=="P4"|bonus_share$treatment=="S4", 1) # K = 4 , value is 1  
bonus_share$matching <- replace(bonus_share$matching, bonus_share$treatment=="P4"|bonus_share$treatment=="P2", 1) # P, value is 1
#bonus_share$id_matching <- replace(bonus_share$id_matching, bonus_share$treatment=="P4"|bonus_share$treatment=="P2", 1) # P, value is 1
#bonus_share <- bonus_share %>% group_by(session.code, player_unique_ID) %>% mutate(id_actor = group_indices())
# order data
#bonus_share <- bonus_share[order(bonus_share$player_unique_ID),]
# create list for ulam - rethinking package
#bonus_share_all_list <- list(
#  id_session = as.integer(as.factor(bonus_share$session.code)),
#  id_actor = (bonus_share$player_unique_ID),
#  id_treat = as.integer(as.factor(bonus_share$treatment)),
#  k = as.integer(bonus_share$k),
#  matching = as.integer((bonus_share$matching)),
#  id_matching = as.integer((bonus_share$id_matching)),
#  number_of_bonuses = (bonus_share$n_bonuses),
#  number_of_sharing = (bonus_share$n_sharing)
#)


# bonus share half is the clean df - No BOTs, only sharing decisions - for the last 15 rounds of the game --------------------------
#bonus_share_half <- df %>% subset.data.frame( subset = (player.bonus_flag==1 & player.BOT_sharing==0 & subsession.round_number >14),
#                                              select=c(session.code,session.label,
#                                                       treatment,
#                                                       subsession.round_number,
#                                                       player.share_decision,
#                                                       player.bonus_flag,
#                                                       player.accrued_bonuses,
#                                                       player_unique_ID)) %>%
#  group_by(treatment,session.label, session.code, player_unique_ID) %>% 
#  summarize(perc_share=mean(player.share_decision), 
#            n_sharing= sum(player.share_decision),
#            n_bonuses = length(player.bonus_flag), 
#            n_bonuses_received = max(player.accrued_bonuses))
#bonus_share_half$n_bonuses_received <- bonus_share_half$n_bonuses_received - bonus_share_half$n_bonuses
#bonus_share_half$k <- 0
#bonus_share_half$matching <- 0
#bonus_share_half$id_matching <- 2
#bonus_share_half$k <- replace(bonus_share_half$k, bonus_share_half$treatment=="P4"|bonus_share_half$treatment=="S4", 1) # K = 4 , value is 1  
#bonus_share_half$matching <- replace(bonus_share_half$matching, bonus_share_half$treatment=="P4"|bonus_share_half$treatment=="P2", 1) # P, value is #1
#bonus_share_half$id_matching <- replace(bonus_share_half$id_matching, bonus_share_half$treatment=="P4"|bonus_share_half$treatment=="P2", 1) # P, value is 1
#bonus_share_half$id_actor <- as.integer(as.factor(bonus_share_half$participant.label))
# order data
#bonus_share <- bonus_share[order(bonus_share$player_unique_ID),]
# create list for ulam
#bonus_share_half_list <- list(
#  id_session = as.integer(as.factor(bonus_share_half$session.code)),
#  id_actor = (bonus_share_half$player_unique_ID),
#  id_treat = as.integer(as.factor(bonus_share_half$treatment)),
#  k = as.integer(bonus_share_half$k),
#  matching = as.integer((bonus_share_half$matching)),
#  id_matching = as.integer((bonus_share_half$id_matching)),
#  number_of_bonuses = (bonus_share_half$n_bonuses),
#  number_of_sharing = (bonus_share_half$n_sharing)
#)


## Longitudinal-Individual data ------ N obs per player (not-aggregated over rounds) -- Used in summary.R ------------------
bonus_share_longitudinal <- df %>% subset.data.frame( subset = (player.bonus_flag==1 & player.BOT_sharing==0),
                                                      select=c(session.code,
                                                               treatment,
                                                               subsession.round_number,
                                                               participant.id_in_session,
                                                               player.share_decision,
                                                               player.bonus_flag,
                                                               player.accrued_bonuses, 
                                                               player.sex, 
                                                               player.student, 
                                                               player.field_of_studies))
bonus_share_longitudinal <- bonus_share_longitudinal %>% group_by(session.code, participant.id_in_session) %>% mutate(id_actor = cur_group_id()) %>% arrange(id_actor, subsession.round_number) %>% 
  mutate(decision_number = row_number(),
         lagged_1_accrued = lead(player.accrued_bonuses, n=1),
         lagged_2_accrued = lead(player.accrued_bonuses, n=2),
         n_innov_received = lagged_1_accrued - decision_number)

# define variable distinguishing partner vs stranger
bonus_share_longitudinal$partner <- ifelse(bonus_share_longitudinal$treatment=="S2"|bonus_share_longitudinal$treatment=="S4", 0, 1)
bonus_share_longitudinal$K <- ifelse(bonus_share_longitudinal$treatment=="S2"|bonus_share_longitudinal$treatment=="P2", 0, 1)

#bonus_share_longitudinal_K2 <- bonus_share_longitudinal %>%subset.data.frame( subset = (treatment=="P2"|treatment=="S2"))




# database of subjects who got checks right ------------------------

#source("Analysis Scripts/Others/questionnaire_check.R")
#bonus_share_right <- bonus_share %>% filter(participant.label %in% IDs_right$participant#.label)
### ------- treatment-level summary
#bonus_share_right_per_treatment <- bonus_share_right %>% group_by(treatment, id_actor)%#>% 
#  summarise(perc_share=mean(perc_share))
#
#bonus_share_right_per_matching <- bonus_share_right %>% group_by(matching)
#
#data.frame(
#  mean_session=tapply(bonus_share_right_per_treatment$perc_share, #bonus_share_right_per_treatment$treatment, mean),
#  sd_session=tapply(bonus_share_right_per_treatment$perc_share, #bonus_share_right_per_treatment$treatment, sd),
#  median_session = tapply(bonus_share_right_per_treatment$perc_share, #bonus_share_right_per_treatment$treatment, median),
#  Max_Sharing=tapply(bonus_share_right_per_treatment$perc_share, #bonus_share_right_per_treatment$treatment, max),
#  Min_Sharing=tapply(bonus_share_right_per_treatment$perc_share, #bonus_share_right_per_treatment$treatment, min),
#  observations = tapply(bonus_share_right_per_treatment$perc_share, #bonus_share_right_per_treatment$treatment, length)
#)
#data.frame(
#  mean_session=tapply(bonus_share_right_per_matching$perc_share, #bonus_share_right_per_matching$matching, mean),
#  sd_session=tapply(bonus_share_right_per_matching$perc_share, #bonus_share_right_per_matching$matching, sd),
#  median_session = tapply(bonus_share_right_per_matching$perc_share, #bonus_share_right_per_matching$matching, median),
#  Max_Sharing=tapply(bonus_share_right_per_matching$perc_share, #bonus_share_right_per_matching$matching, max),
#  Min_Sharing=tapply(bonus_share_right_per_matching$perc_share, #bonus_share_right_per_matching$matching, min),
#  observations = tapply(bonus_share_right_per_matching$perc_share, #bonus_share_right_per_matching$matching, length)
#)
## boxplots
#ggplot(data=bonus_share_right, aes(x=treatment, y=perc_share))+
#  geom_boxplot()+ylab("% of Sharing")+theme_classic()
### distribution plots
#bonus_share_right_P2 <- bonus_share_right %>% subset.data.frame(subset = treatment=="P2"#)
#bonus_share_right_S2 <- bonus_share_right %>% subset.data.frame(subset = treatment=="S2"#)
#bonus_share_right_P4 <- bonus_share_right %>% subset.data.frame(subset = treatment=="P4"#)
#bonus_share_right_S4 <- bonus_share_right %>% subset.data.frame(subset = treatment=="S4"#)
#list_treatments <- list(bonus_share_right_P2,bonus_share_right_P4,bonus_share_right_S2#,bonus_share_right_S4)
#plots=list()
#for (i in 1:4) {
#  p <- eval(substitute(
#    ggplot(data = list_treatments[[i]],aes(x=list_treatments[[i]]$perc_share), alpha=0.4#)+
#      geom_rug(aes(x = list_treatments[[i]]$perc_share, y = 0), position = #position_jitter(height = 0))+ geom_density(color="darkblue", fill="lightblue")+
#      xlab(paste("Treatment", list_treatments[[i]]$treatment[1], sep= " "))+ 
#      geom_vline(aes(xintercept=mean(list_treatments[[i]]$perc_share)),
#                 color="blue", linetype="dashed", size=1) +
#      annotate("rect",xmin=as.numeric(mean_se(list_treatments[[i]]$perc_share))[2], 
#               xmax=as.numeric(mean_se(list_treatments[[i]]$perc_share))[3], ymin=0, #ymax=Inf, alpha=.5)+
#      xlim(c(0,1))+ylab("Density")+ theme_classic(), 
#    list(i=i)))
#  print(i)
#  print(p)
#  plots[[i]] <- p
#}
#do.call("grid.arrange", c(plots, ncol=2))
#
## ------- round-level summary
#bonus_share_longitudinal_right <- bonus_share_longitudinal %>% filter(participant.label #%in% IDs_right$participant.label)
#bonus_share_longitudinal_treatment_right <- bonus_share_longitudinal_right %>% group_by#(treatment, subsession.round_number)%>% 
#  summarise(perc_share=mean(player.share_decision))
#
### comparison plots
#plot_1_data <- bonus_share_longitudinal_treatment_right %>% subset.data.frame(subset = #treatment=="P2" | treatment=="S2")
#Networks=factor(plot_1_data$treatment)
#time_2 <- ggplot(data=plot_1_data, aes(x=subsession.round_number, y=perc_share))+
#  geom_line(aes(colour=Networks)) + geom_point(aes(col= Networks))+
#  ylim(c(min(bonus_share$perc_share),1))+theme_classic()+xlab("Round")+ylab("% of #Sharing")+
#  geom_smooth(method="lm", aes(colour=Networks), se=F)+ ggtitle("A") + theme(legend#.title = element_blank(), legend.position = "bottom")
#
#plot_2_data <- bonus_share_longitudinal_treatment_right %>% subset.data.frame(subset = #treatment=="P4" | treatment=="S4")
#Networks2=factor(plot_2_data$treatment)
#time_4 <- ggplot(data=plot_2_data, aes(x=subsession.round_number, y=perc_share))+
#  geom_line(aes(colour=Networks2), se=F) + geom_point(aes(col= Networks2))+
#  ylim(c(min(bonus_share$perc_share),1))+theme_classic()+xlab("Round")+ylab("% of #Sharing")+
#  geom_smooth(method="lm",aes(colour=factor(Networks2)), se=F)+ ggtitle("B") + theme#(legend.title = element_blank(), legend.position = "bottom")
#grid.arrange(time_2, time_4, nrow=1)

## this list is used in the reciprocity checks --------------------------------
#bonus_share_Partner <- bonus_share %>% 
#  subset.data.frame(
#    subset = (
#      (treatment == "P2" |
#         treatment == "P4")
#    )
#  )
#
#bonus_share_Partner$id_actor <- as.integer(as.factor(bonus_share_Partner$id_actor))


