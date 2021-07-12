# HP1
# GOAL: HERE I RUN MAIN AND ALTERNATIVE MODELS, ANALYSE RESULTS FROM BOTH AND PRODUCE OUTPUT FOR THE PAPER
# THE CODE IS DIVIDED INTO 2 PARTS. MODELS USING 1) A MERGED DATABASE 2) SPLIT DATABASE.

# 1) merged dataset
## main model ----------------
s1 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- mu_a + a[id_actor]*sigma_a + s[id_session]*sigma_s + b[id_matching]
  ,
  a[id_actor] ~ dnorm(0,1),
  s[id_session] ~ dnorm(0,1),
  b[id_matching] ~ dnorm(0,1),
  mu_a ~ dnorm(0,1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1)  
),
log_lik = T, chains = 1, iter = 9000, warmup = 1000, cores = 4,
data = bonus_share_all_list)

## alternative models (PUT IN THE SUPPLEMENTARY INFORMATION)--------
s1.1 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- mu_a + a[id_actor]*sigma_a + b[id_matching]
  ,
  a[id_actor] ~ dnorm(0,1),
  b[id_matching] ~ dnorm(0,1),
  mu_a ~ dnorm(0,1),
  sigma_a ~ dexp(1)
),
log_lik = T, chains = 1, cores = 4, iter = 9000, warmup = 1000, 
data = bonus_share_all_list)

s1.2 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a + b[id_matching]
  ,
  a ~ dnorm(0,1),
  b[id_matching] ~ dnorm(0,1)
),
log_lik = T, chains = 1, iter = 9000, warmup = 1000, cores = 4,
data = bonus_share_all_list)

## END OF THE MODELS ##

## BEGINNING OF THE ANALYSIS ##
# MODELS COMPARISON
### table A1
comparison_h1 <- as.data.frame(compare(s1, s1.1, s1.2)[,1:5]); 
print(xtable(comparison_h1, type="latex"), file = paste(hp1_dir,"table_compare_waic.tex"),sep="")
### figure A7
png("Analysis Scripts/HP1/Output/SI_compare_waic_h1.png", width = 600, height = 400)
plot(compare(s1, s1.1, s1.2))
dev.off()
### figure A6
png("Analysis Scripts/HP1/Output/SI_beta_comparison.png", width = 600, height = 400)
plot(coeftab(s1, s1.1, s1.2), pars = c("b[1]","b[2]"))
dev.off()

# ANALYSIS OF MAIN MODEL------
# extract samples
s1_samples <- extract.samples(s1)
# compute contrasts
s1_samples$diff_b <- s1_samples$b[,1] - s1_samples$b[,2]
# study distribution of parameter b
b_1 <- precis(s1_samples$b[,1], prob=.95)[1:4]; b_1$HPDI_low <- HPDI(s1_samples$b[,1])[1]; b_1$HPDI_high <- HPDI(s1_samples$b[,1])[2]
b_2 <- precis(s1_samples$b[,2], prob=.95)[1:4]; b_2$HPDI_low <- HPDI(s1_samples$b[,2])[1]; b_2$HPDI_high <- HPDI(s1_samples$b[,2])[2]
diff <- precis(s1_samples$diff, prob=.95)[1:4]; diff$HPDI_low <- HPDI(s1_samples$diff)[1]; diff$HPDI_high <- HPDI(s1_samples$diff)[2]
table_b <- rbind.data.frame(b_1, b_2, diff); print(table_b)
# table 3
print(xtable(table_b, type="latex"), file = paste(hp1_dir,"table_beta_H1.tex", sep=""))
# figure 4
png(filename = paste(hp1_dir,"beta_m.png", sep=""), width = 600, height = 400)
dens(s1_samples$diff, show.HPDI = .95, show.zero = T, main = expression(paste(beta[Partner]-beta[Stranger]))); abline(v=mean((s1_samples$diff)),col="blue")
dev.off()
# study absolute effect (in proba terms)
p_s1 <- apply(link_ulam(s1), 2, mean)
P_actors <- as.data.frame(bonus_share[which(bonus_share$treatment=="P2"|bonus_share$treatment=="P4"),"id_actor"])[,1]
prob_partner <- p_s1[P_actors]
prob_stranger <- p_s1[-P_actors]
diff_prob <- mean(prob_partner) - mean(prob_stranger)
# study log-odds (relative effect)
(exp(precis(s1,pars = "b",2)[1,1]-precis(s1,pars = "b",2)[2,1])) # log odd

# ANALYSIS OF ALTERNATIVE MODELS (SI)--------
# extract samples
s1.1_samples <- extract.samples(s1.1)
# compute contrasts
s1.1_samples$diff_b <- s1.1_samples$b[,1] - s1.1_samples$b[,2]
b_1 <- precis(s1.1_samples$b[,1], prob=.95)[1:4]; b_1$HPDI_low <- HPDI(s1.1_samples$b[,1])[1]; b_1$HPDI_high <- HPDI(s1.1_samples$b[,1])[2]
b_2 <- precis(s1.1_samples$b[,2], prob=.95)[1:4]; b_2$HPDI_low <- HPDI(s1.1_samples$b[,2])[1]; b_2$HPDI_high <- HPDI(s1.1_samples$b[,2])[2]
diff <- precis(s1.1_samples$diff, prob=.95)[1:4]; diff$HPDI_low <- HPDI(s1.1_samples$diff)[1]; diff$HPDI_high <- HPDI(s1.1_samples$diff)[2]
table_b <- rbind.data.frame(b_1, b_2, diff); print(table_b)

# study distribution of parameter b
precis(s1.1_samples$diff)
dens(s1.1_samples$diff, show.HPDI = .95, show.zero = T); abline(v=mean((s1.1_samples$diff)),col="blue")

## END OF ANALYSIS WITH 1 ONLY DATABASE##
#########################################

## 2) SPLIT database - LAst 15 rounds
h1 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- mu_a + a[id_actor]*sigma_a + s[id_session]*sigma_s + b[id_matching]
  ,
  a[id_actor] ~ dnorm(0,1),
  s[id_session] ~ dnorm(0,1),
  b[id_matching] ~ dnorm(0,1),
  mu_a ~ dnorm(0,1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1)
),
log_lik = T, chains = 1, iter = 9000, warmup = 1000, cores = 4,
data = bonus_share_half_list)

h1_samples <- extract.samples(h1)
h1_samples$diff_b <- h1_samples$b[,1] - h1_samples$b[,2]
b_1 <- precis(h1_samples$b[,1], prob=.95)[1:4]; b_1$HPDI_low <- HPDI(h1_samples$b[,1])[1]; b_1$HPDI_high <- HPDI(h1_samples$b[,1])[2]
b_2 <- precis(h1_samples$b[,2], prob=.95)[1:4]; b_2$HPDI_low <- HPDI(h1_samples$b[,2])[1]; b_2$HPDI_high <- HPDI(h1_samples$b[,2])[2]
diff <- precis(h1_samples$diff, prob=.95)[1:4]; diff$HPDI_low <- HPDI(h1_samples$diff)[1]; diff$HPDI_high <- HPDI(h1_samples$diff)[2]
table_b <- rbind.data.frame(b_1, b_2, diff); print(table_b)

## Standard and multilevel models

# K = 2 -----------------------------------
s1.1 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + b[id_treat],
  a[id_actor] ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
log_lik = T, chains = 4, cores = 4,
data = bonus_share_K2_list)

s1.1cml <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + b[id_treat],
  a[id_actor] ~ dnorm(mu_a,sigma_a),
  b[id_treat] ~ dnorm(0,1.5),
  mu_a ~ dnorm(0, 0.1),
  sigma_a ~ dexp(1)
),
log_lik = T, chains = 4, cores = 4,
data = bonus_share_K2_list,
constraints=list(sigma_a="lower=0"))

s1.1cml_noncentered <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- c + a[id_actor]*sigma_a + b[id_treat],
  a[id_actor] ~ dnorm(0,1),
  b[id_treat] ~ dnorm(0,1.5),
  c ~ dnorm(0, .1),
  sigma_a ~ dexp(1)
),
log_lik = T, chains = 4, cores = 4,
data = bonus_share_K2_list,
constraints=list(sigma_a="lower=0"))

s1.2 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + s[id_session] + b[id_treat],
  s[id_session]~ dnorm(0,sigma_s),
  a[id_actor] ~ dnorm(mu_a,sigma_a),
  b[id_treat] ~ dnorm(0,1.5),
  mu_a ~ dnorm(0, .1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(0)
),
log_lik = T, chains = 4, cores = 4,
data = bonus_share_K2_list)

s1.2non_centered <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- c + a[id_actor]*sigma_a + s[id_session]*sigma_s + b[id_treat],
  c ~ dnorm(0, .1),
  s[id_session]~ dnorm(0,1),
  a[id_actor] ~ dnorm(0,1),
  b[id_treat] ~ dnorm(0,1.5),
  #mu_a ~ dnorm(0, 1),
  #mu_s ~ dnorm(0, 1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(0.1)
),
log_lik = T, chains = 4, cores = 4,
data = bonus_share_K2_list)

# models comparison
plot(compare(s1.1, s1.1cml, s1.2, s1.2non_centered))
plot(coeftab(s1.1, s1.1cml, s1.2, s1.2non_centered), pars=c("b[1]", "b[2]"))
# compare n_eff of the two best models
n_eff_df <- data.frame(var=row.names(precis(s1.2,2)), n_eff_s1.1 = append(append(rep(NA,8), precis(s1.1cml,2)[[5]]),NA),n_eff_s1.2 = precis(s1.2,2)[[5]], diff = n_eff_s1.1 - n_eff_s1.2)
# extract samples
s1.1_samples <- extract.samples(s1.1)
s1.1cml_samples <- extract.samples(s1.1cml)
s1.1cml_noncentered_samples <- extract.samples(s1.1cml_noncentered)
s1.2_samples <- extract.samples(s1.2)
s1.2non_centered_samples <- extract.samples(s1.2non_centered)
# compare sigma session vs. actor
dens(s1.2_samples$sigma_a, xlim = c(-1, 4))
dens(s1.2_samples$sigma_s, add = T, col = "red")

# contrasts
s1.1_contrast <- inv_logit(s1.1_samples$b[,1])-inv_logit(s1.1_samples$b[,2])
s1.1cml_contrast <- inv_logit(s1.1cml_samples$b[,1])-inv_logit(s1.1cml_samples$b[,2])
s1.1cml_noncentered_contrast <- inv_logit(s1.1cml_noncentered_samples$b[,1])-inv_logit(s1.1cml_noncentered_samples$b[,2])
s1.2_contrast <- inv_logit(s1.2_samples$b[,1])-inv_logit(s1.2_samples$b[,2])
s1.2non_centered_contrast <- inv_logit(s1.2non_centered_samples$b[,1])-inv_logit(s1.2non_centered_samples$b[,2])
dens(s1.1_contrast, show.HPDI = .95, show.zero = T); precis(s1.1_contrast); HPDI(s1.1_contrast, prob = .95)
dens(s1.1cml_contrast, show.HPDI = .95, show.zero = T); precis(s1.1cml_contrast); HPDI(s1.1cml_contrast, prob = .95)
dens(s1.1cml_noncentered_contrast, show.HPDI = .95, show.zero = T); precis(s1.1cml_noncentered_contrast); HPDI(s1.1cml_noncentered_contrast, prob = .95)
dens(s1.2_contrast, show.HPDI = .95, show.zero = T); precis(s1.2_contrast); HPDI(s1.2_contrast, prob = .95)
dens(s1.2non_centered_contrast, show.HPDI = .95, show.zero = T); precis(s1.2non_centered_contrast); HPDI(s1.2non_centered_contrast, prob = .95)

# posterior check per treatment
p_s1.1cml <- postcheck(s1.1cml)
P2_actors <- as.data.frame(bonus_share_K2[which(bonus_share_K2$treatment=="P2"),"unique_player_id_2"])
dens(p_s1.1cml$mean[P2_actors$unique_player_id_2], main = "Partner - K = 2",  type = "l", col = "red")
dens(bonus_share_P2$perc_share, add = T)
dens(p_s1.1cml$mean[-P2_actors$unique_player_id_2],main = "Stranger - K = 2",  type = "l", col = "red")
dens(bonus_share_S2$perc_share, add = T)

## K = 4 ------------------------
s1.4 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + b[id_treat],
  a[id_actor] ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
log_lik = T, chains = 4, cores = 4,
data = bonus_share_K4_list)

s1.4cml <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + b[id_treat],
  a[id_actor] ~ dnorm(mu_a,sigma_a),
  b[id_treat] ~ dnorm(0,1.5),
  mu_a ~ dnorm(0, 0.1),
  sigma_a ~ dexp(1)
),
log_lik = T, chains = 4, cores = 4,
data = bonus_share_K4_list)

s1.5 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + s[id_session] + b[id_treat],
  s[id_session]~ dnorm(0,sigma_s),
  a[id_actor] ~ dnorm(mu_a,sigma_a),
  b[id_treat] ~ dnorm(0, 1.5),
  mu_a ~ dnorm(0, 0.1),
  sigma_a ~ dexp(1),
  sigma_s ~ dexp(1)
),
log_lik = T, chains = 4, cores = 4, 
data = bonus_share_K4_list)

# models comparison
plot(compare(s1.4, s1.4cml, s1.5))
plot(coeftab(s1.4, s1.4cml, s1.5), pars=c("b[1]", "b[2]"))
# extract samples
s1.4_samples <- extract.samples(s1.4)
s1.4cml_samples <- extract.samples(s1.4cml)
s1.5_samples <- extract.samples(s1.5)
# contrasts
s1.4_contrast <- inv_logit(s1.4_samples$b[,1])-inv_logit(s1.4_samples$b[,2])
s1.4cml_contrast <- inv_logit(s1.4cml_samples$b[,1])-inv_logit(s1.4cml_samples$b[,2])
s1.5_contrast <- inv_logit(s1.5_samples$b[,1])-inv_logit(s1.5_samples$b[,2])
dens(s1.4_contrast, show.HPDI = .95, show.zero = T); precis(s1.4_contrast); HPDI(s1.4_contrast, prob = .95)
dens(s1.4cml_contrast, show.HPDI = .95, show.zero = T); precis(s1.4cml_contrast); HPDI(s1.4cml_contrast, prob = .95)
dens(s1.5_contrast, show.HPDI = .95, show.zero = T); precis(s1.5_contrast); HPDI(s1.5_contrast, prob = .95)

# posterior check per treatment
p_s1.4cml <- postcheck(s1.4cml)
P4_actors <- as.data.frame(bonus_share_K4[which(bonus_share_K4$treatment=="P4"),"unique_player_id_2"])
dens(p_s1.4cml$mean[P4_actors$unique_player_id_2])
dens(bonus_share_P4$perc_share, add = T)
dens(p_s1.4cml$mean[-P4_actors$unique_player_id_2])
dens(bonus_share_S4$perc_share, add = T)

