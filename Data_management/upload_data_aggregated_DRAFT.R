## Individual-Aggregated data ------ 1 obs per player (aggregated over rounds)
## data used for HP1 and HP2

# bonus share is the clean df - No BOTs, only sharing decisions - 
bonus_share <- df %>% subset.data.frame( subset = (player.bonus_flag==1 & player.BOT_sharing==0),
                                         select=c(session.code,session.label,
                                                  treatment,
                                                  subsession.round_number,
                                                  unique_player_id, 
                                                  player.share_decision,
                                                  player.bonus_flag,
                                                  player.accrued_bonuses)) %>%
  group_by(treatment,session.label, session.code, unique_player_id) %>% 
  summarize(perc_share=mean(player.share_decision), 
            n_sharing= sum(player.share_decision),
            n_bonuses = length(player.bonus_flag), 
            n_bonuses_received = max(player.accrued_bonuses))
bonus_share$n_bonuses_received <- bonus_share$n_bonuses_received - bonus_share$n_bonuses

# bonus share half is the clean df - No BOTs, only sharing decisions - for the last 15 rounds of the game
bonus_share_half <- df %>% subset.data.frame( subset = (player.bonus_flag==1 & player.BOT_sharing==0 & subsession.round_number >14),
                                         select=c(session.code,session.label,
                                                  treatment,
                                                  subsession.round_number,
                                                  unique_player_id, 
                                                  player.share_decision,
                                                  player.bonus_flag,
                                                  player.accrued_bonuses)) %>%
  group_by(treatment,session.label, session.code, unique_player_id) %>% 
  summarize(perc_share=mean(player.share_decision), 
            n_sharing= sum(player.share_decision),
            n_bonuses = length(player.bonus_flag), 
            n_bonuses_received = max(player.accrued_bonuses))
bonus_share$n_bonuses_received <- bonus_share$n_bonuses_received - bonus_share$n_bonuses

# these are bonus share divided per treatment individually
bonus_share_P2 <- bonus_share %>% subset.data.frame(subset = treatment=="P2")
bonus_share_S2 <- bonus_share %>% subset.data.frame(subset = treatment=="S2")
bonus_share_P4 <- bonus_share %>% subset.data.frame(subset = treatment=="P4")
bonus_share_S4 <- bonus_share %>% subset.data.frame(subset = treatment=="S4")

# the lists used in HP1
bonus_share_K2 <- bonus_share %>% 
  subset.data.frame(
    subset = (
        (treatment == "P2" |
           treatment == "S2")
    )
  )
bonus_share_K2$unique_player_id_2 <- as.integer(as.factor(bonus_share_K2$unique_player_id))
bonus_share_K2 <- bonus_share_K2[order(bonus_share_K2$unique_player_id_2),]

bonus_share_K2_list <- list(
  id_session = as.integer(as.factor(bonus_share_K2$session.code)),
  id_actor = bonus_share_K2$unique_player_id_2,
  id_treat = as.integer(as.factor(bonus_share_K2$treatment)),
  number_of_bonuses = bonus_share_K2$n_bonuses,
  number_of_sharing = bonus_share_K2$n_sharing
)

bonus_share_K4 <-
  bonus_share %>% subset.data.frame(
    subset = (
        (treatment == "P4" |
           treatment == "S4")
    ),
)

bonus_share_K4$unique_player_id_2 <- as.integer(as.factor(bonus_share_K4$unique_player_id))
bonus_share_K4 <- bonus_share_K4[order(bonus_share_K4$unique_player_id_2),]
bonus_share_K4_list <- list(
  id_session = as.integer(as.factor(bonus_share_K4$session.code)),
  id_actor = (bonus_share_K4$unique_player_id_2),
  id_treat = as.integer(as.factor(bonus_share_K4$treatment)),
  number_of_bonuses = bonus_share_K4$n_bonuses,
  number_of_sharing = bonus_share_K4$n_sharing
)

# lists used in HP2
bonus_share$k <- 0
bonus_share$matching <- 0
bonus_share$id_matching <- 2
bonus_share$k <- replace(bonus_share$k, bonus_share$treatment=="P4"|bonus_share$treatment=="S4", 1) # K = 4 , value is 1  
bonus_share$matching <- replace(bonus_share$matching, bonus_share$treatment=="P4"|bonus_share$treatment=="P2", 1) # P, value is 1
bonus_share$id_matching <- replace(bonus_share$id_matching, bonus_share$treatment=="P4"|bonus_share$treatment=="P2", 1) # P, value is 1
bonus_share$id_actor <- as.integer(as.factor(bonus_share$unique_player_id))

bonus_share <- bonus_share[order(bonus_share$id_actor),]
bonus_share_all_list <- list(
  id_session = as.integer(as.factor(bonus_share$session.code)),
  id_actor = (bonus_share$id_actor),
  id_treat = as.integer(as.factor(bonus_share$treatment)),
  k = as.integer(bonus_share$k),
  matching = as.integer((bonus_share$matching)),
  id_matching = as.integer((bonus_share$id_matching)),
  number_of_bonuses = (bonus_share$n_bonuses),
  number_of_sharing = (bonus_share$n_sharing)
)

bonus_share_half$k <- 0
bonus_share_half$matching <- 0
bonus_share_half$id_matching <- 2
bonus_share_half$k <- replace(bonus_share_half$k, bonus_share_half$treatment=="P4"|bonus_share_half$treatment=="S4", 1) # K = 4 , value is 1  
bonus_share_half$matching <- replace(bonus_share_half$matching, bonus_share_half$treatment=="P4"|bonus_share_half$treatment=="P2", 1) # P, value is 1
bonus_share_half$id_matching <- replace(bonus_share_half$id_matching, bonus_share_half$treatment=="P4"|bonus_share_half$treatment=="P2", 1) # P, value is 1
bonus_share_half$id_actor <- as.integer(as.factor(bonus_share_half$unique_player_id))

bonus_share_half_list <- list(
  id_session = as.integer(as.factor(bonus_share_half$session.code)),
  id_actor = (bonus_share_half$id_actor),
  id_treat = as.integer(as.factor(bonus_share_half$treatment)),
  k = as.integer(bonus_share_half$k),
  matching = as.integer((bonus_share_half$matching)),
  id_matching = as.integer((bonus_share_half$id_matching)),
  number_of_bonuses = (bonus_share_half$n_bonuses),
  number_of_sharing = (bonus_share_half$n_sharing)
)

## this list is used in checks for HP2
bonus_share_Partner <- bonus_share %>% 
  subset.data.frame(
    subset = (
      (treatment == "P2" |
         treatment == "P4")
    )
  )

bonus_share_Partner$id_actor <- as.integer(as.factor(bonus_share_Partner$id_actor))

## Longitudinal-Individual data ------ N obs per player (not-aggregated over rounds)
bonus_share_longitudinal <- df %>% subset.data.frame( subset = (player.bonus_flag==1 & player.BOT_sharing==0),
                                         select=c(session.code,
                                                  treatment,
                                                  subsession.round_number,
                                                  participant.label,
                                                  unique_player_id,
                                                  player.share_decision,
                                                  player.bonus_flag,
                                                  player.accrued_bonuses))
bonus_share_longitudinal$unique_player_id <- as.integer(as.factor(bonus_share_longitudinal$participant.label))
bonus_share_longitudinal_K2 <- bonus_share_longitudinal %>%subset.data.frame( subset = (treatment=="P2"|treatment=="S2"))
  