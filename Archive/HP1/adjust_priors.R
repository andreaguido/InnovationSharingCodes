# Adjusting Priors ------------------------
## Priors on intercept --------------------
p1 <- quap(
  alist(
    number_of_sharing ~ dbinom(number_of_bonuses, p),
    logit(p) <- a,
    a ~ dnorm(0, 1.5)
  ),
  data = bonus_share_K2_list
  
)

## extract prior predictive simulations
prior <- extract.prior(p1, n=1e4)
p <- inv_logit(prior$a)
dens(p, adj=.2)

## Priors on beta --------------------
p1.1 <- quap(
  alist(
    number_of_sharing ~ dbinom(number_of_bonuses, p),
    logit(p) <- a + b[id_treat],
    a ~ dnorm(0, 1.5),
    b[id_treat] ~ dnorm(0,0.5)
  ),
  data = bonus_share_K2_list
  
)

## extract prior predictive simulations
prior <- extract.prior(p1.1, n=1e4)
p <- sapply(1:2, function(k) inv_logit(prior$a + prior$b[,k]))
dens(p[,1], main ="b[partner]") ; mean(p[,1])
dens(p[,2]); mean(p[,2])
dens(abs(p[,1]-p[,2]), adj=.2);mean(abs(p[,1]-p[,2]));sd((abs(p[,1]-p[,2])))

## Priors on session intercepts --------------------
p1.2 <- quap(
  alist(
    number_of_sharing ~ dbinom(number_of_bonuses, p),
    logit(p) <- a + s + b[id_treat],
    a ~ dnorm(0, 1.5),
    s ~ dnorm(0,1.5),
    b[id_treat] ~ dnorm(0,0.5)
  ),
  data = bonus_share_K2_list
  
)

## extract prior predictive simulations
prior <- extract.prior(p1.2, n=1e4)
p <- sapply(1:2, function(k) inv_logit(prior$a + prior$b[,k]))
dens(p[,1], main ="b[partner]") ; mean(p[,1])
dens(p[,2]); mean(p[,2])
dens(abs(p[,1]-p[,2]), adj=.2);mean(abs(p[,1]-p[,2]));sd((abs(p[,1]-p[,2])))

