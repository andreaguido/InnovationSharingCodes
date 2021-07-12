## Standard regression (no clusters)

# K = 2
## standard model ----------------
s0 <- quap(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a,
  a ~ dnorm(0,1.5)
),
data = bonus_share_K2_list)

s1 <- quap(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a + b[id_treat],
  a ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
data = bonus_share_K2_list)
s1.1 <- quap(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + b[id_treat],
  a[id_actor] ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
data = bonus_share_K2_list)
s1.2 <- quap(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + s[id_session] + b[id_treat],
  s[id_session]~ dnorm(0,1.5),
  a[id_actor] ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
data = bonus_share_K2_list)
s1.3 <- quap(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a + s[id_session] + b[id_treat],
  s[id_session]~ dnorm(0,1.5),
  a ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
data = bonus_share_K2_list)

# models comparison
plot(compare(s0,s1, s1.1, s1.2, s1.3))
# extract posterior
s1_post_K2_individual <- extract.samples(s1, n = 1e5)
s1.1_post_K2_individual <- extract.samples(s1.1, n = 1e5)
s1.2_post_K2_individual <- extract.samples(s1.2, n = 1e5)
s1.3_post_K2_individual <- extract.samples(s1.3, n = 1e5)
# Contrasts
s1_post_K2_individual$diff_p <- inv_logit(s1_post_K2_individual$a + s1_post_K2_individual$b[, 1]) - inv_logit(s1_post_K2_individual$a + s1_post_K2_individual$b[, 2]); dens(s1_post_K2_individual$diff_p, show.HPDI = .95, main = "S1 - Diff in Prob P2 vs S2")
s1_post_K2_individual$diff_b <- (s1_post_K2_individual$b[, 1]) - (s1_post_K2_individual$b[, 2]); dens(s1_post_K2_individual$diff_b, show.HPDI = .95, main = "S1 - Diff. in Betas P2 vs S2")
precis(s1_post_K2_individual$diff_p, prob = .95)
# for s1.1
s1.1_post_K2_individual$diff_p <- inv_logit(s1.1_post_K2_individual$a + s1.1_post_K2_individual$b[, 1]) - inv_logit(s1.1_post_K2_individual$a +s1.1_post_K2_individual$b[, 2]); dens(s1.1_post_K2_individual$diff_p, show.HPDI = .95, main = "S1.1 - Treatment Effect P2 vs S2")
s1.1_post_K2_individual$diff_b <- s1.1_post_K2_individual$b[, 1] - s1.1_post_K2_individual$b[, 2]; dens(s1.1_post_K2_individual$diff_b, show.HPDI = .95, main = "S1.1 - Treatment Effect P2 vs S2")
precis(s1.1_post_K2_individual$diff_p, prob = .95)
precis(s1.1_post_K2_individual$diff_b, prob = .95)
# for s1.2
s1.2_post_K2_individual$diff_p <- inv_logit(s1.2_post_K2_individual$a +s1.2_post_K2_individual$b[, 1]) - inv_logit(s1.2_post_K2_individual$a +s1.2_post_K2_individual$b[, 2]); dens(s1.2_post_K2_individual$diff_p, show.HPDI = .95, main = "Treatment Effect P2 vs S2")
s1.2_post_K2_individual$diff_b <- (s1.2_post_K2_individual$b[, 1]) - (s1.2_post_K2_individual$b[, 2]); dens(s1.2_post_K2_individual$diff_b, show.HPDI = .95, main = "Treatment Effect P2 vs S2")
precis(s1.2_post_K2_individual$diff_p, prob = .95)
precis(s1.2_post_K2_individual$diff_b, prob = .95)
# for s1.3
s1.3_post_K2_individual$diff_p <- inv_logit(s1.3_post_K2_individual$a +s1.3_post_K2_individual$b[, 1]) - inv_logit(s1.3_post_K2_individual$a +s1.3_post_K2_individual$b[, 2]); dens(s1.3_post_K2_individual$diff_p, show.HPDI = .95, main = "Treatment Effect P2 vs S2")
s1.3_post_K2_individual$diff_b <- (s1.3_post_K2_individual$b[, 1]) - (s1.3_post_K2_individual$b[, 2]); dens(s1.3_post_K2_individual$diff_b, show.HPDI = .95, main = "Treatment Effect P2 vs S2")
precis(s1.3_post_K2_individual$diff_p, prob = .95)
precis(s1.3_post_K2_individual$diff_b, prob = .95)

#compare the betas among models
plot(coeftab(s1, s1.1, s1.2, s1.3), pars=c("b[1]", "b[2]"))
# compare the contrasts among models
contrasts_K2 <- data.frame(S1= s1_post_K2_individual$diff_b, S1.1= s1.1_post_K2_individual$diff_b, S1.2 =s1.2_post_K2_individual$diff_b, S1.3= s1.3_post_K2_individual$diff_b) 
plot(precis(contrasts_K2, depth = 2))

# K = 4 ------

## standard model ----------------
s3 <- quap(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a + b[id_treat],
  a[id_actor] ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
data = bonus_share_K4_list)
s3 <- quap(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + b[id_treat],
  a[id_actor] ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
data = bonus_share_K4_list)
s3 <- quap(alist(
  number_of_sharing ~ dbinom(number_of_bonuses, p),
  logit(p) <- a[id_actor] + b[id_treat],
  a[id_actor] ~ dnorm(0,1.5),
  b[id_treat] ~ dnorm(0, 0.5)
),
data = bonus_share_K4_list)
# evaluate results
precis(s1.5, 2)
# extract posterior
post_K4_individual <- extract.samples(s1.5)
# treat effect
labs <- c("P4", "S4")
plot(precis(s1.5, depth = 2, pars="b"), labels=labs)
p_P4_individual <- inv_logit(post_K4_individual$b[, 1]+post_K4_individual$a); dens(p_P4_individual, show.HPDI = .95, main = "Prob.Sharing P4", xlim=c(0,1))
p_S4_individual <- inv_logit(post_K4_individual$b[, 2]+post_K4_individual$a); dens(p_S4_individual, show.HPDI = .95, main = "Prob.Sharing S4", xlim=c(0,1))
diff_p <- inv_logit(post_K4_individual$b[, 1]) - inv_logit(post_K4_individual$b[, 2]); dens(diff_p, show.HPDI = .95, main = "Treatment Effect P4 vs S4")
precis(diff_p, prob = .95)
