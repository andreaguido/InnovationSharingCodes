# summary statistics from experimental data

# general settings
nofrounds <- 30

# 
#--------- sanity checks ----------------------
# SUMMARY #
# we find no differences across treatments in terms of active players (engaged in the combinatorial task) and
# automatic sharing decisions (timeout). We have only 3 and 5 cases respectively across all sessions players and periods. 

## active rounds per player
### combinations STAGE I
#combo_active_table <- df %>% 
#  subset.data.frame(select=c(session.label, treatment,
#                             subsession.round_number,
#                             player.active, 
#                             participant.id_in_session,
#                             unique_player_id)) %>% 
#  group_by(treatment) %>% 
#  summarize(ACTIVE= mean(player.active))
### summarize
#tapply(combo_active_table$ACTIVE, combo_active_table$treatment, summary)
### test
#chisq.test(combo_active_table$ACTIVE, combo_active_table$treatment, simulate.p.value = T)

### sharing decisions STAGE II
#sharing_active_table <- df %>% 
#  subset.data.frame(select=c(session.label, treatment,
#                             subsession.round_number,
#                             player.BOT_sharing, 
#                             participant.id_in_session)) %>% 
#  group_by(treatment) %>% 
#  summarize(NOTACTIVE= mean(player.BOT_sharing))
### summarize
#tapply(sharing_active_table$NOTACTIVE, sharing_active_table$treatment, summary)
### test
#chisq.test(sharing_active_table$NOTACTIVE, sharing_active_table$treatment, simulate.p.value = T)


#--------- sharing data ----------
## ------- session-level summary ------

#data.frame(
#  mean_session=tapply(bonus_share$perc_share, bonus_share$session.label, mean),
#  sd_session=tapply(bonus_share$perc_share, bonus_share$session.label, sd),
#  Max_Sharing=tapply(bonus_share$perc_share, bonus_share$session.label, max),
#  Min_Sharing=tapply(bonus_share$perc_share, bonus_share$session.label, min),
#  observations = tapply(bonus_share$perc_share, bonus_share$session.label, length),
#  bonuses_round = tapply(bonus_share$n_bonuses, bonus_share$session.label, mean)
#  )


## ------- treatment-level summary ------
#bonus_share_per_treatment <- bonus_share %>% group_by(treatment)%>% 
#  summarise(perc_share=mean(perc_share), sd_perc_share=sd(perc_share, na.rm = T), N = n())

#bonus_share_per_matching <- bonus_share %>% group_by(matching)
bonus_share_K2 <- bonus_share%>% subset.data.frame(subset = k == 0)
bonus_share_K4 <- bonus_share%>% subset.data.frame(subset = k == 1)


se <- function(x){sd(x)/sqrt(length(x))}
d1 <- bonus_share %>% group_by(treatment) %>% summarise(
  mean_session=mean(perc_share),
  se_session=se(perc_share),
  sd_session=sd(perc_share),
  median_session = median(perc_share),
  Max_Sharing=max(perc_share),
  Min_Sharing=min(perc_share),
  observations = length(perc_share)
)
d1$treatment <- factor(d1$treatment, levels=c("P2", "S2", "P4", "S4"))

d2=bonus_share %>% group_by(matching) %>% summarise(
  mean_session=mean(perc_share),
  se_session=se(perc_share),
  sd_session=sd(perc_share),
  median_session = median(perc_share),
  Max_Sharing=max(perc_share),
  Min_Sharing=min(perc_share),
  observations = length(perc_share)
)
d2$matching <- ifelse(d2$matching==0, "Stranger", "Partner")

t.test(bonus_share$perc_share~bonus_share$matching)
cohens_d(bonus_share$perc_share~bonus_share$matching)

# barplots

## partner vs. stranger
ggplot(data=d2, aes(x=matching, y=mean_session, fill= matching))+
  geom_bar(stat="identity", fill="blue", alpha=0.7, width = 0.4)+
  ylab("% Subjects choosing ''Sharing''")+xlab("Treatments")+geom_col(colour = "black") +
  geom_errorbar(aes(x=matching, ymin=mean_session-se_session, ymax=mean_session+se_session), width=0.1, colour="black", alpha=0.9, size=1) + 
  theme_light() + geom_text(aes(label = round(mean_session, 2), vjust = 2))+scale_fill_brewer(palette = "Pastel1")

## all treatments together
ggplot(data=d1)+
  geom_bar(aes(x=treatment, y=mean_session), stat="identity", fill="blue", alpha=0.7, width=0.4)+ylab("% Subjects choosing ''Sharing''")+theme_bw()+xlab("Treatments")+
  geom_errorbar(aes(x=treatment, ymin=mean_session-se_session, ymax=mean_session+se_session), width=0.1, colour="black", alpha=0.9, size=1) + theme_light()

ggplot(data=d1 %>% subset.data.frame(subset = treatment %in% c("P2", "S2")), aes(x=treatment, y=mean_session, fill = treatment))+
  geom_bar(stat="identity", fill="blue", alpha=0.7, width=0.4)+
  ylab("% Subjects choosing ''Sharing''")+xlab("Treatments")+geom_col(colour = "black") + ylim(c(0, 1))+
  geom_errorbar(aes(x=treatment, ymin=mean_session-se_session, ymax=mean_session+se_session), width=0.1, colour="black", alpha=0.9, size=1) + theme_light() +
  geom_text(aes(label = round(mean_session, 2), vjust = 2))+scale_fill_brewer(palette = "Pastel1")

ggplot(data=d1 %>% subset.data.frame(subset = treatment %in% c("P4", "S4")), aes(x=treatment, y=mean_session, fill = treatment))+
  geom_bar(stat="identity", fill="blue", alpha=0.7, width=0.4)+
  ylab("% Subjects choosing ''Sharing''")+xlab("Treatments")+geom_col(colour = "black") + ylim(c(0, 1))+
  geom_errorbar(aes(x=treatment, ymin=mean_session-se_session, ymax=mean_session+se_session), width=0.1, colour="black", alpha=0.9, size=1) + theme_light() +
  geom_text(aes(label = round(mean_session, 2), vjust = 3))+scale_fill_brewer(palette = "Pastel1")

t.test(bonus_share_K2$perc_share ~ bonus_share_K2$treatment)
t.test(bonus_share_K4$perc_share ~ bonus_share_K4$treatment)
chisq.test(bonus_share$perc_share, bonus_share$treatment)
cohens_d(bonus_share_K2$perc_share ~ bonus_share_K2$treatment)
cohens_d(bonus_share_K4$perc_share ~ bonus_share_K4$treatment)

## ------- distribution plots ----
#bonus_share_P2 <- bonus_share %>% subset.data.frame(subset = treatment=="P2")
#bonus_share_S2 <- bonus_share %>% subset.data.frame(subset = treatment=="S2")
#bonus_share_P4 <- bonus_share %>% subset.data.frame(subset = treatment=="P4")
#bonus_share_S4 <- bonus_share %>% subset.data.frame(subset = treatment=="S4")
#list_treatments <- list(bonus_share_P2,bonus_share_P4,bonus_share_S2,bonus_share_S4)
#plots=list()
#for (i in 1:4) {
#  p <- eval(substitute(
#    ggplot(data = list_treatments[[i]],aes(x=list_treatments[[i]]$perc_share), alpha=0.4)+
#      geom_rug(aes(x = list_treatments[[i]]$perc_share, y = 0), position = position_jitter(height = 0))+ geom_density(color="darkblue", fill#="lightblue")+
#      xlab(paste("Treatment", list_treatments[[i]]$treatment[1], sep= " "))+ 
#      geom_vline(aes(xintercept=mean(list_treatments[[i]]$perc_share)),
#                 color="blue", linetype="dashed", size=1) +
#      annotate("rect",xmin=as.numeric(mean_se(list_treatments[[i]]$perc_share))[2], 
#               xmax=as.numeric(mean_se(list_treatments[[i]]$perc_share))[3], ymin=0, ymax=Inf, alpha=.5)+
#      xlim(c(0,1))+ylab("Density")+ theme_classic(), 
#    list(i=i)))
#  print(i)
#  print(p)
#  plots[[i]] <- p
#}
#
#png("Analysis Scripts/HP1/Output/dens.png", width=580, height = 405)
#do.call("grid.arrange", c(plots, ncol=2))
#dev.off()


## ------- round-level summary ------
bonus_share_longitudinal_treatment <- bonus_share_longitudinal %>% group_by(treatment, subsession.round_number)%>% 
  summarise(perc_share=mean(player.share_decision))
bonus_share_longitudinal_matching <- bonus_share_longitudinal %>% mutate(matching = ifelse(treatment == "P2" | treatment == "P4", "Partner", "Stranger")) %>%
  group_by(matching, subsession.round_number)%>% 
  summarise(perc_share=mean(player.share_decision))

## first-round analysis
## among all treatments
first_round_sharing <- bonus_share_longitudinal %>%
  subset.data.frame(subset = subsession.round_number==1) %>% group_by(treatment) %>% summarise(perc_share=mean(player.share_decision), 
                                                                       se = se(player.share_decision),
                                                                       N=n())
chi_test1 <- bonus_share_longitudinal %>% subset.data.frame(subset = subsession.round_number==1) 
chisq.test(chi_test1$player.share_decision,chi_test1$treatment)

## effect of connectivity 2 vs = 4
## Partner
first_round_sharing_P <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==1 & (treatment=="P2" | treatment=="P4")) %>% group_by(treatment) %>% summarise(perc_share=mean(player.share_decision), 
                                                                                             se = se(player.share_decision),
                                                                                             N=n())
wilcox_test1 <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==1 & (treatment=="P2" | treatment=="P4"))
t.test(wilcox_test1$player.share_decision~wilcox_test1$treatment)
## Stranger
first_round_sharing_S <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==1 
                    & (treatment=="S2" | treatment=="S4"))
t.test(first_round_sharing_S$player.share_decision~first_round_sharing_S$treatment)
## effect of matching
## K = 2
first_round_sharing_K2 <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==1 
                    & (treatment=="P2" | treatment=="S2"))
t.test(first_round_sharing_K2$player.share_decision~first_round_sharing_K2$treatment)
first_round_sharing_K2 %>% group_by(treatment) %>% summarise(mean(player.share_decision),
                                                             SE=sd(player.share_decision)/sqrt(length(player.share_decision)),
                                                             LB = mean(player.share_decision-2*sd(player.share_decision)/sqrt((length(player.share_decision)))),
                                                             UB = mean(player.share_decision+2*sd(player.share_decision)/sqrt((length(player.share_decision)))))
## K = 4
first_round_sharing_K4 <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==1 & 
                      (treatment=="P4" | treatment=="S4"))
t.test(first_round_sharing_K4$player.share_decision~first_round_sharing_K4$treatment)
first_round_sharing_K4 %>% group_by(treatment) %>% 
  summarise(mean(player.share_decision),
  SE=sd(player.share_decision)/sqrt(length(player.share_decision)),
  LB = mean(player.share_decision-2*sd(player.share_decision)/
              sqrt((length(player.share_decision)))),
  UB = mean(player.share_decision+2*sd(player.share_decision)/
              sqrt(length(player.share_decision))))

## comparison round 15
## among all treatments
fifteen_round_sharing <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==15)
fifteen_round_sharing$k <- ifelse(fifteen_round_sharing$treatment=="P2" | fifteen_round_sharing$treatment=="S2", 2, 4)
chisq.test(fifteen_round_sharing$player.share_decision,fifteen_round_sharing$k)
t.test(fifteen_round_sharing$player.share_decision~fifteen_round_sharing$k)

fifteen_round_sharing_K2 <- bonus_share_longitudinal %>% subset.data.frame(subset = subsession.round_number==15 & treatment %in% c("P2", "S2"))
t.test(fifteen_round_sharing_K2$player.share_decision~fifteen_round_sharing_K2$treatment)
## comparison round 30
## among all treatments
last_round_sharing <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==30)
chisq.test(last_round_sharing$player.share_decision,last_round_sharing$treatment)
## P2 vs S2
last_round_sharing <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==30 & (treatment == "P2"|treatment == "S2"))
t.test(last_round_sharing$player.share_decision~last_round_sharing$treatment)
## P4 vs S4
last_round_sharing <- bonus_share_longitudinal %>% 
  subset.data.frame(subset = subsession.round_number==30 & (treatment == "P4"|treatment == "S4"))
t.test(last_round_sharing$player.share_decision~last_round_sharing$treatment)

## summary last-half part
second_half_sharing <- bonus_share_longitudinal_treatment %>%  subset.data.frame(subset = subsession.round_number>15)
data.frame(
  mean_session=tapply(second_half_sharing$perc_share, second_half_sharing$treatment, mean),
  sd_session=tapply(second_half_sharing$perc_share, second_half_sharing$treatment, sd),
  median_session = tapply(second_half_sharing$perc_share, second_half_sharing$treatment, median),
  Max_Sharing=tapply(second_half_sharing$perc_share, second_half_sharing$treatment, max),
  Min_Sharing=tapply(second_half_sharing$perc_share, second_half_sharing$treatment, min),
  observations = tapply(second_half_sharing$perc_share, second_half_sharing$treatment, length)
)

## comparison plots
plot_1_data <- bonus_share_longitudinal_treatment %>% subset.data.frame(subset = treatment=="P2" | treatment=="S2")
Networks=factor(plot_1_data$treatment)
time_2 <- ggplot(data=plot_1_data, aes(x=subsession.round_number, y=perc_share))+
  geom_line(aes(colour=Networks)) + geom_point(aes(col= Networks))+
  ylim(c(min(plot_1_data$perc_share),1))+theme_classic()+xlab("Round")+ylab("% Subjects choosing ''Sharing''")+
  geom_smooth(method="lm", aes(colour=Networks), se=F)+ ggtitle("A") + theme(legend.title = element_blank(), legend.position = "bottom")

plot_2_data <- bonus_share_longitudinal_treatment %>% subset.data.frame(subset = treatment=="P4" | treatment=="S4")
Networks2=factor(plot_2_data$treatment)
time_4 <- ggplot(data=plot_2_data, aes(x=subsession.round_number, y=perc_share))+
  geom_line(aes(colour=Networks2), se=F) + geom_point(aes(col= Networks2))+
  ylim(c(0.4,1))+theme_classic()+xlab("Round")+ylab("")+
  geom_smooth(method="lm",aes(colour=factor(Networks2)), se=F)+ ggtitle("B") + theme(legend.title = element_blank(), legend.position = "bottom")

png("HP1/Output/evo_time.png", width = 580, height = 405)
grid.arrange(time_2, time_4, nrow=1)
dev.off()

time_matching <- ggplot(data=bonus_share_longitudinal_matching, aes(x=subsession.round_number, y=perc_share))+
  geom_line(aes(colour=matching)) + geom_point(aes(col= matching))+
  ylim(c(0.4,1))+theme_classic()+xlab("Round")+ylab("% Subjects choosing ''Sharing''")+
  geom_smooth(method="lm",aes(colour=factor(matching)), se=F) + theme(legend.title = element_blank())

#Networks=factor(bonus_share_longitudinal_treatment$treatment)
#ggplot(data=bonus_share_longitudinal_treatment, aes(x=subsession.round_number, y=perc_share))+
#  ylim(c(min(bonus_share_longitudinal_treatment$perc_share),1))+theme_classic()+xlab("Round")+ylab("% of Sharing")+
#  geom_smooth(method="lm",aes(colour=Networks))+ geom_line(aes(colour=Networks))

# preliminary regressions # -----------
library(arm)

# 1. Partner vs. Stranger (HP1) -----
# prepare data
# define variable distinguishing partner vs stranger
bonus_share_longitudinal$partner <- ifelse(bonus_share_longitudinal$treatment=="S2"|bonus_share_longitudinal$treatment=="S4", 0, 1)
bonus_share_longitudinal$K <- ifelse(bonus_share_longitudinal$treatment=="S2"|bonus_share_longitudinal$treatment=="P2", 0, 1)

model0 <- glmer(player.share_decision~partner + (1|id_actor), data = bonus_share_longitudinal,family=binomial(link="logit"))
model0.1 <- glmer(player.share_decision~partner + subsession.round_number + (1|id_actor), data = bonus_share_longitudinal,family=binomial(link="logit"))
model0.2 <- glmer(player.share_decision~partner + subsession.round_number*partner + (1|id_actor), data = bonus_share_longitudinal,family=binomial(link="logit"))

# n. decision of individual instead of round (robustness)
model0.3 <- glmer(player.share_decision~partner + n_innovations_found + (1|session.code) + (1|id_actor), data = bonus_share_longitudinal,family=binomial(link="logit"))
  
# print tables
tab_model(model0, model0.1, model0.2, show.ci = 0.95, show.aic = T, show.r2 = F, pred.labels = c("Intercept", "Partner Matching", "Round", "Round*Partner Matching"))
stargazer(model0, model0.1, model0.2, 
          title = "Logit regression of sharing decisions across Partner and Stranger matching protocols", covariate.labels = c("Partner Matching", "Round", "Round*Partner Matching", "Constant"), 
          dep.var.caption = "Sharing decision", align = T, dep.var.labels.include = F, type = "latex", out = "table1.tex")

# 2. Interaction connectivity and partner
model1 <- glmer(player.share_decision~treatment + (1|id_actor), data = bonus_share_longitudinal,family=binomial)
model1.1 <- glmer(player.share_decision~partner + partner:K + (1|id_actor), data = bonus_share_longitudinal,family=binomial)
model1.2 <- glmer(player.share_decision~partner + partner:K + subsession.round_number + (1|id_actor), data = bonus_share_longitudinal,family=binomial)
#model3 <- glmer(player.share_decision~treatment + subsession.round_number + (1|id_actor), data = bonus_share_longitudinal,family=binomial)
#model3.1 <- glmer(player.share_decision~treatment + subsession.round_number*treatment + (1|id_actor), data = bonus_share_longitudinal, family=binomial)
# n. decision of individual instead of round (robustness)
model1.3 <- glmer(player.share_decision~treatment + n_innovations_found + (1|id_actor), data = bonus_share_longitudinal,family=binomial(link="logit"))

# simulate posteriors
sim_model1 <- coef(sim(model1))
# extract treatment constants
P2_constant <- sim_model1$fixef[,"(Intercept)"]
S2_constant <- P2_constant + sim_model1$fixef[,"treatmentS2"]
difference_P2_S2 <- P2_constant - S2_constant

P4_constant <- P2_constant + sim_model1$fixef[,"treatmentP4"]
S4_constant <- P2_constant + sim_model1$fixef[,"treatmentS4"]
difference_P4_S4 <- P4_constant - S4_constant

diff_total <- difference_P2_S2 - difference_P4_S4

# predictions in probability
predictions <- predict(model1, type = "response")

# test for autocorrelation 
bonus_share_longitudinal_lag <- bonus_share_longitudinal %>% group_by(id_actor) %>% mutate(lagged_1_share = lag(player.share_decision, n=1),lagged_2_share = lag(player.share_decision, n=2))
model4 <- glmer(player.share_decision~treatment + subsession.round_number*treatment + lagged_1_share + lagged_2_share + (1|session.code)+ (1|id_actor), data = bonus_share_longitudinal_lag, family=binomial)
model5 <- glmer(player.share_decision~partner + subsession.round_number*partner + lagged_1_share + lagged_2_share + (1|session.code)+ (1|id_actor), data = bonus_share_longitudinal_lag, family=binomial)

# main tables
tab_model(model0, model0.1, model0.2, show.ci = .95, show.icc = T, show.aic = T, show.r2 = F, show.re.var = T, transform = NULL)
tab_model(model1,model3, model3.1, show.ci = .95, show.re.var = T, show.icc = F, show.aic = T, show.r2 = F, transform = NULL, pred.labels = c("Intercept", "P4","S2","S4", "Round", "Round*P4", "Round*S2", "Round*S4"), 
          dv.labels = c("Sharing decision","Sharing decision","Sharing decision"))
stargazer(model1, model3, model3.1, 
          title = "Mixed Effects Logit regression of sharing decisions across treatments", 
          covariate.labels = c("P4","S2","S4", "Round", "Round*P4", "Round*S2", "Round*S4"), 
          dep.var.caption = "Sharing decision", align = T, dep.var.labels.include = F, type = "latex", out = "table2.tex")

# change in reference treatment (P4 instead of P2)
bonus_share_longitudinal2 <- within(bonus_share_longitudinal, treatment <- relevel(as.factor(bonus_share_longitudinal$treatment), ref = 2))
model1_P4 <- glmer(player.share_decision~treatment + (1|session.code) + (1|id_actor), data = bonus_share_longitudinal2,family=binomial)
model3_P4 <- glmer(player.share_decision~treatment + subsession.round_number + (1|session.code)+ (1|id_actor), data = bonus_share_longitudinal2,family=binomial)
model3.1_P4 <- glmer(player.share_decision~treatment + subsession.round_number*treatment + (1|session.code)+ (1|id_actor), data = bonus_share_longitudinal2, family=binomial)

# paternoster test to compare S2 from model1 and S4 from model1_P4
pater <- (as.numeric(fixef(model1)["treatmentS2"])-as.numeric(fixef(model1_P4)["treatmentS4"]))/sqrt(as.numeric(se.fixef(model1)["treatmentS2"]^2) + as.numeric(se.fixef(model1_P4)["treatmentS4"]^2))
pval_pater <- pnorm(-abs(pater))

# create lagged variables and test for autocorrelation
bonus_share_longitudinal_lag2 <- bonus_share_longitudinal2 %>% group_by(id_actor) %>% mutate(lagged_1_share = lag(player.share_decision, n=1),lagged_2_share = lag(player.share_decision, n=2))
model4_P4 <- glmer(player.share_decision~treatment + subsession.round_number*treatment + lagged_1_share + lagged_2_share + (1|session.code)+ (1|id_actor), data = bonus_share_longitudinal_lag2, family=binomial)
tab_model(model1_P4,model3_P4, model3.1_P4, model4_P4, show.ci = F, show.re.var = F, show.icc = F, transform = NULL, show.aic = T)

stargazer(model1, model3, model3.1, model1_P4, model3_P4, model3.1_P4,
          title = "Mixed Effects Logit regression of sharing decisions across treatments",
          covariate.labels = c("P4","P2","S2","S4", "Round", "Round*P4", "Round*P2", "Round*S2", "Round*S4", "Constant"),
          dep.var.caption = "Sharing decision", align = T, dep.var.labels.include = F, type = "latex", out = "table2_all.tex")
tab_model(model1, model3, model3.1, model1_P4, model3_P4, model3.1_P4,
          title = "Mixed Effects Logit regression of sharing decisions across treatments",
          dv.labels = "Sharing decision")
plot_model(model1, type="pred", title = "Probability of sharing", axis.title = c("Treatment", "Pr(Sharing)"))
  ## ------- reciprocal behaviour -----

bonus_share_longitudinal <- bonus_share_longitudinal %>% group_by(id_actor) %>% mutate(lagged_1_accrued = lead(player.accrued_bonuses, n=1),lagged_2_accrued = lead(player.accrued_bonuses, n=2), K = ifelse(treatment== "P4" | treatment == "S4", 4, 2))
bonus_share_longitudinal <- bonus_share_longitudinal %>% mutate(n_innovations_found = row_number(), n_innov_received = lagged_1_accrued - n_innovations_found)
ggplot(data=bonus_share_longitudinal, aes(x=n_innov_received, y=player.share_decision))+
  stat_smooth(aes(colour= treatment), method="lm", se = F)+ xlab("Innovations Received")+ylab("Sharing %")+
  geom_point(aes(colour= treatment))


ggplot(data=bonus_share_K4, aes(x=n_bonuses_received, y=perc_share))+
  stat_smooth(aes(colour= treatment), se = F, method="lm")+ xlab("Innovations Received")+ylab("Sharing %")+
  geom_point(aes(colour= treatment))

model_rec1 <- lmer(perc_share~n_bonuses_received + n_bonuses_received*treatment + (1|session.code), data = bonus_share_K2)
summary(model_rec1)

model_rec2 <- lmer(perc_share~n_bonuses_received + n_bonuses_received*treatment + (1|session.code), data = bonus_share_K4)
summary(model_rec2)

std_var <- function(x){ (mean(x, na.rm=T)-x)/sd(x, na.rm=T)}
bonus_share_longitudinal <- bonus_share_longitudinal %>% mutate(std_n_innov_received = std_var(n_innov_received))
model_rec_all0 <- glmer(player.share_decision~ K + n_innov_received*K + (1|session.code) + (1|id_actor), data = bonus_share_longitudinal, family=binomial)
summary(model_rec_all0)
model_rec_all1 <- glmer(player.share_decision~ treatment + (1|session.code) + (1|id_actor), data = bonus_share_longitudinal, family=binomial)
summary(model_rec_all1)
model_rec_all2 <- glmer(player.share_decision~ n_innov_received + treatment + (1|session.code) + (1|id_actor), data = bonus_share_longitudinal, family=binomial)
summary(model_rec_all2)
model_rec_all3 <- glmer(player.share_decision~ n_innov_received*treatment + (1|session.code) + (1|id_actor), data = bonus_share_longitudinal, family=binomial)
summary(model_rec_all3)

tab_model(model_rec_all0, show.ci = F, show.re.var = F, show.icc = F, transform = NULL, show.aic = T)
tab_model(model_rec_all0, model_rec_all3, show.ci = F, show.re.var = F, show.icc = F, transform = NULL, show.aic = T)
stargazer(model_rec_all0, model_rec_all3, 
          title = "Mixed effects Logit models with varying intercepts at the session and individual levels.",
          dep.var.caption   = "Sharing_t", dep.var.labels.include = F, 
          covariate.labels = c("K", "N. Innovation received_{t-1}", "K*N. Innovation received_{t-1}", "P4", "S2", "S4", "N. Innovation received_{t-1}*P4", "N. Innovation received_{t-1}*S2", "N. Innovation received_{t-1}*S4"), align=T, type="latex",out="reciprocity.tex")
plot_model(model_rec_all3, type="pred", terms=c("n_innov_received [all]", "treatment"), axis.title = c("Number Innovations received t-1", "Probability sharing in t"), title = "")

# table 3
print(xtable(table_b, type="latex"), file = paste(hp1_dir,"table_beta_H1.tex", sep=""))
xtable()
