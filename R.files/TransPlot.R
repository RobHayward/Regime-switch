# Create plots of the transition probabilities
library(depmixS4) 
set.seed(3)
inv <- c("HUF", "PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
col <- c("-3sd", "-2sd", "-1sd", "Mean", "+1sd","+2sd", "+3sd")
complist2 <- matrix(NA, nrow = length(inv), ncol = length(col), 
                   dimnames = list(inv, col))
# = "NOK"
# add the x values for the computation of the exogenous shock
x <- seq(-3, 3, 1)
for(i in inv){
  tempfile <- forp(i, "EUR", 1)
  tempfx <- tempfile$data$p
mod <- depmix(tempfx ~ 1, nstates = 2, transition = ~ scale(USD1MD), 
               data = da)               
fm <- fit(mod, verbose = TRUE, emcontrol = em.control(maxit = 1000))
if(getpars(fm)[11] > getpars(fm)[13]){
    # select state 1 parameters
  selection = 1
} else {
  selection = 2
}
  a <- fm@transition[[selection]]@parameters[[1]][3]
  b <- fm@transition[[selection]]@parameters[[1]][4]
ya <- a + b*x
if(selection == 1){
for(j in seq(1, 7, 1)){
  complist2[i, j] <- round((1/(1+exp(-ya[j]))), 4)
}
} else  {
  for(j in seq(1, 7, 1)){
  complist2[i, j] <- 1 - (round((1/(1+exp(-ya[j]))), 4))
}
}
}
complist2
# now create the latex.  Change the title for each save as txt. 
comptab2 <- xtable(complist2, caption = "Transition probabilities conditional 
                   on international risk aversion", label = 
                     "tabref:comptab", digits = c(2, 4, 4, 4, 4, 4, 4, 4))
comptab2
write.csv(complist2, './Data/1MD.csv')
#--------------------
# This does not look that interesting at the moment
par(mfrow = c(2,1))
for(i in 1:2){
a <- fm@transition[[i]]@parameters[[1]][3]
b <- fm@transition[[i]]@parameters[[1]][4]
x <- seq(-3, 3, 0.1)
ya <- a + b*x
y <- 1/(1+exp(-ya))
title <- paste("Transition probability from state ", i, " to state", 2, sep = "")
plot(x, y, type = 'l', xlab = "Standard deviations from mean", 
     ylab = "Probability", main = title)
}
}
summary(fm)
1/(1+exp(77.2035 + (116.7436*3)))
