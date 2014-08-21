# Create plots of the transition probabilities
library(depmixS4) 
set.seed(3)
inv <- c("HUF", "PLN", "CZK", "RON",  "RUB", "BGN", 
         "NOK", "ISK", "UAH", "HRK", "TRY")
i = "HUF"
for(i in INV){
  tempfile <- forp(i, "EUR", 1)
  tempfx <- tempfile$data$p
mod <- depmix(tempfx ~ 1, nstates = 2, transition = ~ scale(VIX), 
               data = da)               
fm <- fit(mod, verbose = TRUE, emcontrol = em.control(maxit = 1000))
par(mfrow = c(2,1))
for(i in 1:2){
a <- fm@transition[[i]]@parameters[[1]][3]
a
b <- fm@transition[[i]]@parameters[[1]][4]
b
x <- seq(-3, 3, 0.1)
ya <- a + b*x
ya
y <- 1/(1+exp(-ya))
title <- paste("Transition probability for state ", i, sep = "")
plot(x, y, type = 'l', xlab = "Standard deviations from mean", 
     ylab = "Probability", main = title)
}
summary(fm)
fm@transition
[[1]][3]
1/(1+exp(9.605878))
plot((fm@transition[[1]])@x, (fm@transition[[1]])@y)
coeff
@parameters)
i <- 1
