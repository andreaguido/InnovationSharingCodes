compute_yule <- function(data_assortment, player){
  # Yule's Q is (ad-bc)/(ad+bc) (cfr. Borgatti 2019 lecture)
  
  # check type of player j
  if (data_assortment$type[player]==1){
    a <- data_assortment$type_1_links[player] # ij are of same group and have tie
    b <- length(which(data_assortment$type==1))-1 # ij are of same group and no tie
    c <- data_assortment$type_2_links[player] # ij in different groups and have tie
    d <- length(which(data_assortment$type==2)) # ij different groups and no tie
    yule <- (a*d - b*c)/(a*d + b*c)
    return(yule)
  } else {
    a <- data_assortment$type_2_links[player]
    b <- length(which(data_assortment$type==2))-1
    c <- data_assortment$type_1_links[player]
    d <- length(which(data_assortment$type==1))
    yule <- (a*d - b*c)/(a*d + b*c)
    return(yule)    
  }
}
