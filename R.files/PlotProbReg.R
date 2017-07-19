# PLot carry-retruns and the probability of being in a regime. 
library(depmixS4) 
set.seed(3)
inv <- c( "RON",  "RUB", "BGN", 
          "NOK", "ISK", "UAH", "HRK", "TRY")
col <- c("-3sd", "-2sd", "-1sd", "Mean", "+1sd","+2sd", "+3sd")
complist2 <- matrix(NA, nrow = length(inv), ncol = length(col), 
                    dimnames = list(inv, col))
i = "PLN"
# for(i in inv){
  tempfile <- forp(i, "EUR", 1)
  tempfx <- tempfile$data$p
  mod <- depmix(tempfx ~ 1, nstates = 2, transition = ~ scale(VIX), 
                data = da)               
  fm <- fit(mod, verbose = TRUE, emcontrol = em.control(maxit = 1000))
#  if(getpars(fm)[7] > getpars(fm)[9]){
#    # select state 1 parameters
#    selection = 1
#  } else {
#    selection = 2
#  }
pst <- posterior(fm)
pst$Date <- index(tempfx)
pdf("Figures/PLNEUR2.pdf", paper= "a4r", title = "HUF-EUR Carry")
title <- paste(i, " carry log returns", sep = "")
par(mfrow = c(3,1))
plot(tempfx, main = title, type = 'l')
abline(h = 1)
plot(pst[,3] ~ pst$Date, type = 'l', main = "Probability in State 1: Calm", 
     xlab = 'Date')
plot(pst[,2] ~ pst$Date, type = 'l', main = "Probability in State 2: Crisis", 
     xlab = 'Date')
dev.off()

