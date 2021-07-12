p2 = as.data.frame(bonus_share %>% subset.data.frame(, select = perc_share, subset = treatment== "P2"))
s2 = as.data.frame(bonus_share %>% subset.data.frame(, select = perc_share, subset = treatment== "S2"))
plot(ecdf(p2[,1]), main = "CDF plot", xlab= "Sharing Rates", ylab="Cumulative Proportion", cex =0)
lines(ecdf(s2[,1]),cex =0, col="red")
legend('bottomright', 
       legend=c("P2","S2"),
       col=c("black","red"), 
       pch=15)
s4 = as.data.frame(bonus_share %>% subset.data.frame(, select = perc_share, subset = treatment== "S4"))
p4 = as.data.frame(bonus_share %>% subset.data.frame(, select = perc_share, subset = treatment== "P4"))
plot(ecdf(p4[,1]), main = "CDF plot", xlab= "Sharing Rates", ylab="Cumulative Proportion", cex =0)
lines(ecdf(s4[,1]),cex =0, col="red")
legend('bottomright', 
       legend=c("P4","S4"),
       col=c("black","red"), 
       pch=15)
