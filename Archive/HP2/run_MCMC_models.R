# HP2
s2 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- mu_a + a[id_actor]*sigma_a + s[id_session]*sigma_s  + c[id_treat],
  s[id_session]~ dnorm(0,1),
  a[id_actor] ~ dnorm(0,1),
  c[id_treat] ~ dnorm(0,1),
  mu_a ~ dnorm(0,1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1)
),
log_lik = T, chains = 1, iter = 9000, warmup = 1000, cores = 4, #control=list(adapt_delta = .99),
data = bonus_share_all_list)

s2.1 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- mu_a + a[id_actor]*sigma_a + c[id_treat],
  a[id_actor] ~ dnorm(0,1),
  c[id_treat] ~ dnorm(0,1),
  mu_a ~ dnorm(0,1),
  sigma_a ~ dexp(1)
  #sigma_s ~ dexp(1)
),
log_lik = T, chains = 1, iter = 9000, warmup = 1000, cores = 4, #control=list(adapt_delta = .99),
data = bonus_share_all_list)


s2.2 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- mu_a + a[id_actor]*sigma_a + s[id_session]*sigma_s + b*matching + c*matching*k,
  s[id_session]~ dnorm(0,1),
  a[id_actor] ~ dnorm(0,1),
  b ~ dnorm(0, 1),
  c ~ dnorm(0,1),
  mu_a ~ dnorm(0,1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1)
),
log_lik = T, chains = 1, iter = 9000, warmup = 1000, cores = 4, 
data = bonus_share_all_list)

# models comparison  ----------------
comparison_h2 <- as.data.frame(compare(s2, s2.1))[,1:5];
print(xtable(comparison_h2, type="latex"), file = paste(hp2_dir,"table_compare_waic_h2.tex"),sep="")
png(filename = "Analysis Scripts/HP2/Output/SI_waic_comparison_h2.png", width = 600, height = 400)
plot(compare(s2, s2.1))
dev.off()
png(filename = "Analysis Scripts/HP2/Output/SI_compare_c_h2.png", width = 600, height = 400)
plot(coeftab(s2, s2.1), pars = c("c[1]","c[2]","c[3]","c[4]"))
dev.off()


# ANALYSIS OF THE MAIN MODEL -------------------------------
# extract samples
s2_samples <- extract.samples(s2)
# analysis of intercepts
precis(as.data.frame(s2_samples$c)); # all together

# construct contrasts 
## note: id_treat code meaning: 1 - P2, 2 - P4, 3 - S2, 4 - S4
## goal: compute difference btw treat effect:  {c[2] - c[4]} - {c[1] - c[3]}
## HP: we hypothesize that the difference btw Tr.Eff is negative, meaning that
##     a higher connectivity deteriorates the effect of being matched with same partner
png(filename = "Analysis Scripts/HP2/Output/contrasts.png")
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE),
       widths=c(1,1), heights=c(1,2))
s2_samples$diff_k2 <- s2_samples$c[,1] - s2_samples$c[,3]; dens(s2_samples$diff_k2, show.HPDI = .95, show.zero = T, main = "a"); precis(s2_samples$diff_k2); HPDI(s2_samples$diff_k2, .95)
s2_samples$diff_k4 <- s2_samples$c[,2] - s2_samples$c[,4]; dens(s2_samples$diff_k4, show.HPDI = .95, show.zero = T, main = "b"); precis(s2_samples$diff_k4); HPDI(s2_samples$diff_k4, .95)
s2_samples$diff_c <- s2_samples$diff_k4 - s2_samples$diff_k2; dens(s2_samples$diff_c, show.HPDI = .95, show.zero = T, main = "c"); precis(s2_samples$diff_c); HPDI(s2_samples$diff_c, prob = .95)
dev.off()
# produce tables
c_1 <- precis(s2_samples$c[,1], prob=.95)[1:4]; c_1$HPDI_low <- HPDI(s2_samples$c[,1], .95)[1]; c_1$HPDI_high <- HPDI(s2_samples$c[,1],.95)[2]
c_2 <- precis(s2_samples$c[,2], prob=.95)[1:4]; c_2$HPDI_low <- HPDI(s2_samples$c[,2], .95)[1]; c_2$HPDI_high <- HPDI(s2_samples$c[,2],.95)[2]
c_3 <- precis(s2_samples$c[,3], prob=.95)[1:4]; c_3$HPDI_low <- HPDI(s2_samples$c[,3], .95)[1]; c_3$HPDI_high <- HPDI(s2_samples$c[,3],.95)[2]
c_4 <- precis(s2_samples$c[,4], prob=.95)[1:4]; c_4$HPDI_low <- HPDI(s2_samples$c[,4], .95)[1]; c_4$HPDI_high <- HPDI(s2_samples$c[,4],.95)[2]
diffc13 <- precis(s2_samples$diff_k2, prob=.95)[1:4]; diffc13$HPDI_low <- HPDI(s2_samples$diff_k2, .95)[1]; diffc13$HPDI_high <- HPDI(s2_samples$diff_k2,.95)[2]
diffc24 <- precis(s2_samples$diff_k4, prob=.95)[1:4]; diffc24$HPDI_low <- HPDI(s2_samples$diff_k4, .95)[1]; diffc24$HPDI_high <- HPDI(s2_samples$diff_k4,.95)[2]
diffc <- precis(s2_samples$diff_c, prob=.95)[1:4]; diffc$HPDI_low <- HPDI(s2_samples$diff_c, .95)[1]; diffc$HPDI_high <- HPDI(s2_samples$diff_c,.95)[2]
table_c <- rbind.data.frame(c_1, c_2, c_3, c_4, diffc13, diffc24, diffc); print(table_c)
print(xtable(table_c, type="latex"), file=paste(hp2_dir,"tableh2.tex"), sep="")

# absolute effect 

# relative effect (odds change)
exp(precis(s2_samples$diff_k2)[1,1])
exp(precis(s2_samples$diff_k4)[1,1])
exp(precis(s2_samples$diff_c)[1,1]) #odds change is about 70% -- odds(k=4) == odds(when k2)*.30
# Model's posterior check (per treatment)
p_s2 <- postcheck(s2)
P2_actors <- as.data.frame(bonus_share[which(bonus_share$treatment=="P2"),"id_actor"])
S2_actors <- as.data.frame(bonus_share[which(bonus_share$treatment=="S2"),"id_actor"])
P4_actors <- as.data.frame(bonus_share[which(bonus_share$treatment=="P4"),"id_actor"])
S4_actors <- as.data.frame(bonus_share[which(bonus_share$treatment=="S4"),"id_actor"])
dens(p_s2$mean[P2_actors$id_actor], main = "Partner - K = 2",  type = "l", col = "red")
dens(bonus_share_P2$perc_share, add = T)
dens(p_s2$mean[S2_actors$id_actor], main = "Stranger - K = 2",  type = "l", col = "red")
dens(bonus_share_S2$perc_share, add = T)
dens(p_s2$mean[P4_actors$id_actor], main = "Partner - K = 4",  type = "l", col = "red")
dens(bonus_share_P4$perc_share, add = T)
dens(p_s2$mean[S4_actors$id_actor], main = "Stranger - K = 4",  type = "l", col = "red")
dens(bonus_share_S4$perc_share, add = T)

# ANALYSIS OF ALTERNATIVE MODELS-----------

# model s2.1
# extract samples
s2.1_samples <- extract.samples(s2.1)
# construct contrasts
s2.1_samples$diff_k2 <- s2.1_samples$c[,1] - s2.1_samples$c[,3]
s2.1_samples$diff_k4 <- s2.1_samples$c[,2] - s2.1_samples$c[,4]
s2.1_samples$diff_c <- s2.1_samples$diff_k4 - s2.1_samples$diff_k2
dens(s2.1_samples$diff_c, show.HPDI = .95, show.zero = T); precis(s2.1_samples$diff_c); HPDI(s2.1_samples$diff_c, prob = .95)
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE),
       widths=c(1,1), heights=c(1,2))
s2.1_samples$diff_k2 <- s2.1_samples$c[,1] - s2.1_samples$c[,3]; dens(s2.1_samples$diff_k2, show.HPDI = .95, show.zero = T, main = "a"); precis(s2.1_samples$diff_k2); HPDI(s2.1_samples$diff_k2, .95)
s2.1_samples$diff_k4 <- s2.1_samples$c[,2] - s2.1_samples$c[,4]; dens(s2.1_samples$diff_k4, show.HPDI = .95, show.zero = T, main = "b"); precis(s2.1_samples$diff_k4); HPDI(s2.1_samples$diff_k4, .95)
s2.1_samples$diff_c <-  s2.1_samples$diff_k4 - s2.1_samples$diff_k2; dens(s2.1_samples$diff_c, show.HPDI = .95, show.zero = T, main = "c"); precis(s2.1_samples$diff_c); HPDI(s2.1_samples$diff_c, prob = .95)
# produce tables
c_1 <- precis(s2.1_samples$c[,1], prob=.95)[1:4]; c_1$HPDI_low <- HPDI(s2.1_samples$c[,1], .95)[1]; c_1$HPDI_high <- HPDI(s2.1_samples$c[,1],.95)[2]
c_2 <- precis(s2.1_samples$c[,2], prob=.95)[1:4]; c_2$HPDI_low <- HPDI(s2.1_samples$c[,2], .95)[1]; c_2$HPDI_high <- HPDI(s2.1_samples$c[,2],.95)[2]
c_3 <- precis(s2.1_samples$c[,3], prob=.95)[1:4]; c_3$HPDI_low <- HPDI(s2.1_samples$c[,3], .95)[1]; c_3$HPDI_high <- HPDI(s2.1_samples$c[,3],.95)[2]
c_4 <- precis(s2.1_samples$c[,4], prob=.95)[1:4]; c_4$HPDI_low <- HPDI(s2.1_samples$c[,4], .95)[1]; c_4$HPDI_high <- HPDI(s2.1_samples$c[,4],.95)[2]
diffc13 <- precis(s2.1_samples$diff_k2, prob=.95)[1:4]; diffc13$HPDI_low <- HPDI(s2.1_samples$diff_k2, .95)[1]; diffc13$HPDI_high <- HPDI(s2.1_samples$diff_k2,.95)[2]
diffc24 <- precis(s2.1_samples$diff_k4, prob=.95)[1:4]; diffc24$HPDI_low <- HPDI(s2.1_samples$diff_k4, .95)[1]; diffc24$HPDI_high <- HPDI(s2.1_samples$diff_k4,.95)[2]
diffc <- precis(s2.1_samples$diff_c, prob=.95)[1:4]; diffc$HPDI_low <- HPDI(s2.1_samples$diff_c, .95)[1]; diffc$HPDI_high <- HPDI(s2.1_samples$diff_c,.95)[2]
table_c_2 <- rbind.data.frame(c_1, c_2, c_3, c_4, diffc13, diffc24, diffc); print(table_c)
print(xtable(table_c, type="latex"), file=paste(hp2_dir,"tableh2_model2.tex"), sep="")

# model s2.2
# extract samples
s2.2_samples <- extract.samples(s2.2)
# analise coefficient c
dens(s2.2_samples$c, show.HPDI = .95, show.zero = T); precis(as.data.frame(s2.2_samples$c)); HPDI(s2.2_samples$c, .95)
