# HP1 - Static vs Non-Static Networks

#  Models of Sharing Rates per treatment -------
flist <- alist(
  n_sharing ~ dbinom(n_bonuses, p),
  p~dunif(0,1)
)

## K = 2
m0_P2 <- quap(
  flist, data=bonus_share_P2
)
precis(m0_P2)
samples_m0_P2=extract.samples(m0_P2)
#dens(samples_m0_P2, show.HPDI = .95)
HPDI(samples_m0_P2, .95)

m0_S2 <- quap(
  flist, data=bonus_share_S2
)
precis(m0_S2)
samples_m0_S2=extract.samples(m0_S2)
#dens(samples_m0_S2, show.HPDI = .95)
HPDI(samples_m0_S2, .95)

## K = 4
m0_P4 <- quap(
  flist, data=bonus_share_P4
)
precis(m0_P4)
samples_m0_P4=extract.samples(m0_P4)
#dens(samples_m0_P4, show.HPDI = .95)
HPDI(samples_m0_P4, .95)

m0_S4 <- quap(
  flist, data=bonus_share_S4
)
precis(m0_S4)
samples_m0_S4=extract.samples(m0_S4)
#dens(samples_m0_S4, show.HPDI = .95)
HPDI(samples_m0_S4, .95)

#  Individual-level, aggregate Binomial models ------------------


# STANDARD model K = 2 ------------------------------------
m1.2 <- ulam(alist(
    number_of_sharing ~ dbinom(number_of_bonuses, p),
    logit(p) <- a[id_actor] + g[id_session] + b[id_treat],
    a[id_actor] ~ dnorm(0,1),
    g[id_session] ~ dnorm(0,1),
    b[id_treat] ~ dnorm(0, 0.5)
  ),
  data = bonus_share_K2_list,
  chains = 4, log_lik = T, cores = 4)

# summarize results
# evaluate MCMC
traceplot(m1.2)
dev.off()
# evaluate results
precis(m1.2, 2)
# extract posterior
post_K2_individual <- extract.samples(m1.2)
# tendency to share per individual
intercept_share <- inv_logit(post_K2_individual$a)
plot( precis( as.data.frame(intercept_share, ) ) )
# treat effect
labs <- c("P2", "S2")
plot(precis(m1.2, depth = 2, pars="b"), labels=labs)
p_P2_individual <- inv_logit(post_K2_individual$b[, 1]); dens(p_P2_individual, show.HPDI = .95, main = "Prob.Sharing P2", xlim=c(0,1))
p_S2_individual <- inv_logit(post_K2_individual$b[, 2]); dens(p_S2_individual, show.HPDI = .95, main = "Prob.Sharing S2", xlim=c(0,1))
diff_p <- inv_logit(post_K2_individual$b[, 1]) - inv_logit(post_K2_individual$b[, 2]); dens(diff_p, show.HPDI = .95, main = "Treatment Effect P2 vs S2")
precis(diff_p, prob = .95)

# posterior check
simulations_posterior <- postcheck(m1.2)
P2_actors <- as.data.frame(bonus_share_K2[which(bonus_share_K2$treatment=="P2"),"unique_player_id_2"])
dens(simulations_posterior$mean[P2_actors$unique_player_id_2])
dens(bonus_share_P2$perc_share, add = T)
dens(simulations_posterior$mean[-P2_actors$unique_player_id_2])
dens(bonus_share_S2$perc_share, add = T)


# MULTILEVEL-model K = 2 -------------------------
m1.2 <- ulam(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <-   g[id_session] + b[id_treat],
  #a ~ dnorm(0,sigmaa),
  g[id_session] ~ dnorm(0,sigmag),
  b[id_treat] ~ dnorm(0, 0.5),
  #sigmaa ~ dexp(2),
  sigmag ~ dexp(2)
),
data = bonus_share_K2_list,
chains = 4, log_lik = F, cores = 4, warmup=700, iter=1200)

# evaluate MCMC
traceplot(m1.2)
dev.off()
# evaluate results
precis(m1.2, 2)
# extract posterior
post_K2_individual <- extract.samples(m1.2)
# treat effect
labs <- c("P2", "S2")
plot(precis(m1.2, depth = 2, pars="b"), labels=labs)
p_P2_individual <- inv_logit(post_K2_individual$b[, 1]); dens(p_P2_individual, show.HPDI = .95, main = "Prob.Sharing P2", xlim=c(0,1))
p_S2_individual <- inv_logit(post_K2_individual$b[, 2]); dens(p_S2_individual, show.HPDI = .95, main = "Prob.Sharing S2", xlim=c(0,1))
diff_p <- inv_logit(post_K2_individual$b[, 1]) - inv_logit(post_K2_individual$b[, 2]); dens(diff_p, show.HPDI = .95, main = "Treatment Effect P2 vs S2")
precis(diff_p, prob = .95)

# posterior check
simulations_posterior <- postcheck(m1.2)
P2_actors <- as.data.frame(bonus_share_K2[which(bonus_share_K2$treatment=="P2"),"unique_player_id_2"])
dens(simulations_posterior$mean[P2_actors$unique_player_id_2])
dens(bonus_share_P2$perc_share, add = T)
dens(simulations_posterior$mean[-P2_actors$unique_player_id_2])
dens(bonus_share_S2$perc_share, add = T)

# STANDARD model K = 4 --------------------------
# model 
m1.3 <- ulam(
  alist(
    number_of_sharing ~ dbinom(number_of_bonuses, p),
    logit(p) <- a[id_actor] + g[id_session] + b[id_treat],
    a[id_actor] ~ dnorm(0,1),
    g[id_session] ~ dnorm(0,1),
    b[id_treat] ~ dnorm(0, 0.5)
  ),
  data = bonus_share_K4_list,
  chains = 4,
  log_lik = T,
  cores = 4
  
)


# evaluate MCMC
traceplot(m1.3)
dev.off()
# evaluate results
options(max.print = getOption("max.print")+9)
precis(m1.3, 2)
# extract posterior
post_K4_individual <- extract.samples(m1.3)
# tendency to share per individual
intercept_share <- inv_logit(post_K4_individual$a)
plot( precis( as.data.frame(intercept_share) ) , xlim=c(0,1) )
# treat effect
labs <- c("P4", "S4")
plot(precis(m1.3, depth = 2, pars="b"), labels=labs)
p_P4_individual <- inv_logit(post_K4_individual$b[, 1]); dens(p_P4_individual, show.HPDI = .95, main = "Prob.Sharing P4", xlim=c(0,1))
p_S4_individual <- inv_logit(post_K4_individual$b[, 2]); dens(p_S4_individual, show.HPDI = .95, main = "Prob.Sharing S4", xlim=c(0,1))
diff_p <- inv_logit(post_K4_individual$b[, 1]) - inv_logit(post_K4_individual$b[, 2]); dens(diff_p, show.HPDI = .95, main = "Treatment Effect P4 vs S4")
precis(diff_p)

# posterior check
simulations_posterior <- postcheck(m1.3)
P4_actors <- as.data.frame(bonus_share_K4[which(bonus_share_K4$treatment=="P4"),"unique_player_id_2"])
dens(simulations_posterior$mean[P4_actors$unique_player_id_2])
dens(bonus_share_P4$perc_share, add = T)
dens(simulations_posterior$mean[-P4_actors$unique_player_id_2])
dens(bonus_share_S4$perc_share, add = T)


####################################
##### DO NOT CONSIDER ##############
####################################
#Individual-level, multilevel model ---------------------

# indivdual level data --------------
d_K2_individual <-
  df %>% subset.data.frame(
    subset = (
      player.bonus_flag == 1 & player.BOT_sharing == 0 &
        (treatment == "P2" |
           treatment == "S2")
    ),
    select = c(
      session.code,
      session.label,
      treatment,
      subsession.round_number,
      participant.id_in_session,
      participant.label,
      player.share_decision,
      player.bonus_flag
    )
  )

d_K2_individual_list <- list(
  id_session = as.integer(as.factor(d_K2_individual$session.code)),
  id_actor = as.integer(as.factor(d_K2_individual$participant.label)),
  id_treat = as.integer(as.factor(d_K2_individual$treatment)),
  sharing = as.integer(d_K2_individual$player.share_decision)
)

# model -------------------------------
m1.4 <- ulam(alist(
  sharing ~ dbinom(1, p),
  logit(p) <- a[id_actor] + b[id_treat],
  a[id_actor] ~ dnorm(0, 3),
  b[id_treat] ~ dnorm(0, 0.5)
),
data = d_K2_individual_list,
chains = 1, log_lik = T, cores=4
)

# evaluate prior predictions
prior_predictions <- extract_prior_ulam(m1.4)
# evaluate MCMC
traceplot(m1.4)
# evaluate results
precis(m1.4, 2)
# extract posterior
post_K2_individual <- extract.samples(m1.4)
# tendency to share per individual
intercept_share <- inv_logit(post_K2_individual$a)
plot( precis( as.data.frame(intercept_share) ) , xlim=c(0,1) )
# treat effect
labs <- c("P2", "S2")
plot(precis(m1.4, depth = 2, pars="b"), labels=labs)
p_P2_individual <- inv_logit(post_K2_individual$b[, 1]); dens(p_P2_individual, show.HPDI = .95, main = "Prob.Sharing P2", xlim=c(0,1))
p_S2_individual <- inv_logit(post_K2_individual$b[, 2]); dens(p_S2_individual, show.HPDI = .95, main = "Prob.Sharing S2", xlim=c(0,1))
diff_p <- inv_logit(post_K2_individual$b[, 1]) - inv_logit(post_K2_individual$b[, 2]); dens(diff_p, show.HPDI = .95, main = "Treatment Effect P2 vs S2")
precis(diff_p)
