library(ggplot2)

# HP4 - received bonus check 
# if P2 more assorted -> more bonus received
individual_scores <- 
  df %>% subset.data.frame( subset = (player.BOT_sharing==0 & player.bonus_flag == 1),
                            select=c(session.code,
                                     treatment,
                                     subsession.degree,
                                     subsession.round_number,
                                     player.share_decision,
                                     player.bonus_flag,
                                     participant.id_in_session,
                                     player.accrued_bonuses,
                                     player.accrued_score)) %>%
  group_by(session.code, treatment, subsession.degree,participant.id_in_session) %>%
  summarise(bonus_found = sum(player.bonus_flag),
            bonus_shared = sum(player.share_decision),
            accrued_bonus = max(player.accrued_bonuses),
            accrued_score = max(player.accrued_score))
individual_scores <- individual_scores %>% group_by(session.code, participant.id_in_session) %>% mutate(id_actor = group_indices())
individual_scores$received_bonus <- individual_scores$accrued_bonus - individual_scores$bonus_found

individual_scores_longitudinal <- 
  df %>% subset.data.frame( subset = (player.BOT_sharing==0 & player.bonus_flag == 1),
                            select=c(session.code,
                                     treatment,
                                     subsession.degree,
                                     subsession.round_number,
                                     player.share_decision,
                                     player.bonus_flag,
                                     participant.id_in_session,
                                     player.accrued_bonuses,
                                     player.accrued_score)) %>%
  group_by(session.code, treatment, subsession.round_number, subsession.degree,participant.id_in_session) %>%
  summarise(bonus_found = sum(player.bonus_flag),
            bonus_shared = sum(player.share_decision),
            accrued_bonus = max(player.accrued_bonuses),
            accrued_score = max(player.accrued_score))
individual_scores_longitudinal <- individual_scores_longitudinal %>% group_by(session.code, participant.id_in_session) %>% mutate(id_actor = group_indices())
individual_scores_longitudinal$received_bonus <- individual_scores_longitudinal$accrued_bonus - individual_scores_longitudinal$bonus_found
individual_scores_longitudinal$received_bonus <- replace(individual_scores_longitudinal$received_bonus, individual_scores_longitudinal$received_bonus==-1, 0)
# plot scores
ggplot(data=individual_scores, aes(x=treatment, y=accrued_score))+
  geom_boxplot()+ylab("Accrued Score")+theme_classic()
# plot bonuses
ggplot(data=individual_scores, aes(x=treatment, y=accrued_bonus))+
  geom_boxplot()+ylab("Accrued Bonuses")+theme_classic()

# sd r and s over rounds
individual_scores_longitudinal$K = 4*.4*30
individual_scores_longitudinal$K <- replace(individual_scores_longitudinal$K, individual_scores_longitudinal$treatment == "P2" | individual_scores_longitudinal$treatment == "S2", 2*.4*30)
individual_scores_longitudinal$max_s <- 37630
individual_scores_longitudinal$max_s <- replace(individual_scores_longitudinal$max_s, individual_scores_longitudinal$treatment == "P2" | individual_scores_longitudinal$treatment == "S2", 22930)
individual_scores_longitudinal$std_r <- individual_scores_longitudinal$received_bonus/individual_scores_longitudinal$K
individual_scores_longitudinal$std_s <- individual_scores_longitudinal$accrued_score/individual_scores_longitudinal$max_s
individual_scores_longitudinal_summary <- individual_scores_longitudinal %>% group_by(subsession.round_number , treatment) %>% summarise(Efficiency=mean(std_s, na.rm=T))
ggplot(data=individual_scores_longitudinal_summary, aes(x=subsession.round_number, y=Efficiency)) + geom_line(aes(color=treatment)) + geom_smooth()
# sd received bonus
individual_scores$std_r <- individual_scores$received_bonus/individual_scores$K
#HP4_individual <- HP4_individual[order(HP4_individual$session.code,HP4_individual$participant.label, HP4_individual$subsession.round_number),]

# K*Prob
individual_scores$K = 4*.4*30
individual_scores$K <- replace(individual_scores$K, individual_scores$treatment == "P2" | individual_scores$treatment == "S2", 2*.4*30)
individual_scores$max_s <- 37630
individual_scores$max_s <- replace(individual_scores$max_s, individual_scores$treatment == "P2" | individual_scores$treatment == "S2", 22930)

# sd received bonus
individual_scores$std_r <- individual_scores$received_bonus/individual_scores$K
k2=individual_scores %>% subset.data.frame(subset=individual_scores$treatment == "P2" | individual_scores$treatment == "S2")
t.test(k2$std_r ~ k2$treatment)
k4=individual_scores %>% subset.data.frame(subset=individual_scores$treatment == "P4" | individual_scores$treatment == "S4")
t.test(k4$std_r ~ k4$treatment)
barplotdf <- individual_scores %>% group_by(treatment) %>% 
  summarise(mean_received = mean(received_bonus),mean_received_sd = mean(std_r), sd_received_sd = sd(std_r)/sqrt(length(std_r)))
ggplot(data=barplotdf, aes(x=treatment, y=mean_received_sd))+
  geom_bar(stat="identity",fill="steelblue",position=position_dodge())+ylab("Received Bonus (adjusted)")+xlab("Treatment")+
  geom_errorbar(aes(ymin=mean_received_sd-sd_received_sd, ymax=mean_received_sd+sd_received_sd), width=.2,
                position=position_dodge(.9))+
  geom_text(aes(label=round(mean_received_sd,2)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)

# std accrued score
individual_scores$std_s <- individual_scores$accrued_score/individual_scores$max_s
k2=individual_scores %>% subset.data.frame(subset=individual_scores$treatment == "P2" | individual_scores$treatment == "S2")
t.test(k2$std_s ~ k2$treatment)
k4=individual_scores %>% subset.data.frame(subset=individual_scores$treatment == "P4" | individual_scores$treatment == "S4")
t.test(k4$std_s ~ k4$treatment)
all_but_P2 <- individual_scores %>% subset.data.frame(subset=individual_scores$treatment == "P4" | individual_scores$treatment == "S2"| individual_scores$treatment == "S4")

barplotdf <- individual_scores %>% group_by(treatment) %>% 
  summarise(mean_received = mean(std_s), sd_received_sd = sd(std_s)/sqrt(length(std_s)))
ggplot(data=barplotdf, aes(x=as.factor(treatment), y=mean_received))+
  geom_bar(stat="identity",fill="steelblue",position=position_dodge())+ylab("Efficiency")+xlab("Treatment")+
  geom_errorbar(aes(ymin=mean_received-sd_received_sd, ymax=mean_received+sd_received_sd), width=.2,
                position=position_dodge(.9))+  geom_text(aes(label=round(mean_received,2)), vjust=1.6, color="white", position = position_dodge(2.9), size=5)
t.test(individual_scores$std_s~individual_scores$subsession.degree)
t.test(k2$std_s~k2$treatment)
t.test(k4$std_s~k4$treatment)
chisq.test(all_but_P2$std_s, all_but_P2$treatment)
anova(lm(all_but_P2$std_s ~all_but_P2$treatment))

model_eff_0 <- lm(std_s~treatment, data=individual_scores)
model_eff_1 <- lm(std_s~treatment +treatment*subsession.round_number, data=individual_scores_longitudinal)
model_eff_1_lmer <- lmer(std_s~treatment +(1|session.code), data=individual_scores_longitudinal)
model_eff_2_lmer <- lmer(std_s~treatment +treatment*subsession.round_number +(1|session.code), data=individual_scores_longitudinal)

tab_model(model_eff_1_lmer,model_eff_2_lmer, digits = 3)
stargazer(model_eff_1_lmer,model_eff_2_lmer,digit.separate = 3, covariate.labels = c("P4", "S2", "S4", "Round", "P4*Round", "S2*Round", "S4*Round", "Constant"), title = "Mixed effects regression with varying intercepts at the session and individual level.", dep.var.labels.include = F,
          dep.var.caption = "Standardized Efficiency", type="latex", out="efficiency.tex")
##############################################

HP4_score <- 
  df %>% subset.data.frame( subset = (player.BOT_sharing==0 & subsession.round_number == 30) &
                                        (treatment == "P2" | treatment == "P4"),
                            select=c(session.code,
                                     session.label,
                                     treatment,
                                     subsession.round_number,
                                     player.share_decision,
                                     player.bonus_flag,
                                     participant.label,
                                     player.accrued_bonuses,
                                     player.accrued_score))

HP4_score_by_type <- HP4_score %>% group_by(session.label, treatment, player.share_decision) %>%
  summarise(mean_score = mean(player.accrued_score))

HP4_score_coop <- HP4_score_by_type %>% filter(player.share_decision == 1)
HP4_score_def <- HP4_score_by_type %>% filter(player.share_decision == 0)

final_assortment_network$Coop_Score <-   HP4_score_coop$mean_score
final_assortment_network$Def_Score <-   HP4_score_def$mean_score
final_assortment_network$Diff <- final_assortment_network$Coop_Score-final_assortment_network$Def_Score

# compute mean payoff for each session in each round
mp_session_round <- HP4_score %>% group_by(session.code, session.label, subsession.round_number) %>%
  summarise(mean_p = mean(player.accrued_score))

# start the hell loop
sessions_HP4 <- levels(as.factor(mp_session_round$session.code))
HP4_individual$avg <- NA
for (i in sessions_HP4) {
  for (t in 1:30) {
    
    value <- mp_session_round %>% filter(session.code==i & subsession.round_number==t) 
    value <- value$mean_p
    
      HP4_individual$avg <- replace(HP4_individual$avg, 
                                      HP4_individual$session.code==i & HP4_individual$subsession.round_number==t, 
                                      value)
  }
}

# compute deviation from mean
HP4_individual$score_std <- round((HP4_individual$player.accrued_score - HP4_individual$avg)/HP4_individual$K,2)
# clean only bonus flag
HP4_individual <- HP4_individual %>% filter(player.bonus_flag==1)

HP4_individual$n_bonuses_received <- HP4_individual$n_bonuses_received - HP4_individual$n_bonuses
#score_avg <- as.vector(by(HP4_individual$final_score, HP4_individual$treatment, mean))
#HP4_individual$score_std <- HP4_individual$final_score - score_avg[1]
#HP4_individual$score_std <-replace(HP4_individual$score_std, HP4_individual$treatment=="P4", 
#                                   HP4_individual$final_score[HP4_individual$treatment=="P4"] - score_avg[2])

# plots sharing-payoff accrued by treatment
Networks=factor(HP4_individual$treatment)

data_K2 <- HP4_individual %>% filter(treatment=="P2")
data_K2$type <- "C"
data_K2$type <- replace(data_K2$type, data_K2$player.share_decision==0,"D")
ggplot(data=data_K2, aes(x=score_std, fill=type, color = type))+
  geom_density()

data_K4 <- HP4_individual %>% filter(treatment=="P4")
data_K4$type <- "C"
data_K4$type <- replace(data_K4$type, data_K4$player.share_decision==0,"D")
ggplot(data=data_K4, aes(x=score_std, fill=type, color = type))+
  geom_density()

ks.test(data_K2$score_std[data_K2$type=="C"],data_K2$score_std[data_K2$type=="D"])
ks.test(data_K4$score_std[data_K4$type=="C"],data_K4$score_std[data_K4$type=="D"])


HP4_individual_list <- list(
  id_session = as.integer(as.factor(HP4_individual$session.code)),
  id_treatment = as.integer(as.factor(HP4_individual$treatment)),
  id_actor = as.integer(as.factor(HP4_individual$participant.label)),
  share = HP4_individual$player.share_decision,
  K = as.integer(HP4_individual$K),
  #total_score = HP4_individual$final_score,
  std_score = HP4_individual$score_std
)

# prior predictive simulations -----------------
pps <- quap(
  alist(
  total_score ~ dnorm(mu, sigma),
  mu <-  a[id_treatment] + s[id_session] + b*share + c*K*share,
  sigma ~ dunif(2000,8000),
  b[id_treatment] ~ dnorm(0, 150),
  a[id_treatment] ~ dnorm(mu_a, sigma_a),
  s[id_session] ~ dnorm(0, sigma_s),
  mu_a ~ dnorm(0, 8000),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1)
),
data= HP4_individual_list)

pps_prior <- extract.prior(pps, n = 200)

## curves 
N <- length(pps_prior$a[,1])
plot( NULL, xlim= range(HP4_individual$n_sharing), ylim = range(HP4_individual$final_score))
## treatment P2
for (i in 1:N) {
  curve(pps_prior$a[i,1]+pps_prior$b[i,1]*x, from = min(HP4_individual$n_sharing), to = max(HP4_individual$n_sharing), add = T, col= "red")
}


# ULAM -----------------------------------------
m4.1 <- ulam(
  alist(
  std_score ~ dnorm(mu, sigma),
  mu <-  mu_a + a[id_treatment] + s[id_session] + b*share + c*share*K,
  sigma ~ dunif(2000, 8000),
  a[id_treatment] ~ dnorm(0, 1),
  mu_a ~ dnorm(0, 8000)
  b ~ dnorm(0,15)
  c ~ dnorm(0,15)
),
data= HP4_individual_list, log_lik = T, chains = 4, iter = 1000, warmup = 500)

m4.2l <- ulam(
  alist(
    std_score ~ dnorm(mu, sigma),
  mu <-   mu_a + a[id_actor]*sigma_a + s[id_session]*sigma_s + b*K + d*K*share,
  sigma ~ dunif(2000, 8000),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1),
  mu_a ~ dnorm(0, 8000),
  a[id_actor] ~ dnorm(0, 100),
  s[id_session] ~ dnorm(0, 100),
  b ~ dnorm(0,10),
  c ~ dnorm(0,10),
  d ~ dnorm(0,10)
),
data= HP4_individual_list, log_lik  T, chains = =1, iter = 1000, warmup = 500, cores = 4)

m4.3 <- ulam(
  alist(
    std_score ~ dnorm(mu, sigma),
    mu <-  a[id_session] + g[id_actor] + b[id_treatment]*share,
    sigma ~ dunif(2000, 8000),
    a[id_session] ~ dnorm(15000, 3000),
    g[id_actor] ~ dnorm(0, 3000),
    b[id_treatment] ~ dnorm(0,150)
  ),
  data= HP4_individual_list, log_lik = T, chains = 4, iter = 1000, warmup = 500)

m4.4 <- ulam(
  alist(
    std_score ~ dnorm(mu, sigma),
    mu <-  a[id_session] + g[id_actor] + b[id_treatment]*share,
    sigma ~ dunif(2000, 8000),
    a[id_session] ~ dnorm(abar, 3000),
    g[id_actor] ~ dnorm(0, 3000),
    b[id_treatment] ~ dnorm(0,150),
    abar ~ dnorm(15000, 1000)
  ),
  data= HP4_individual_list, log_lik = T, chains = 4, iter = 1000, warmup = 500)


# MODEL COMPARISON
plot(compare(m4.1, m4.2, m4.3, m4.4))
post <- extract.samples(m4.3)
post$diff_b <- post$b[,1] - post$b[,2]; dens(post$diff_b, show.HPDI = .95)
