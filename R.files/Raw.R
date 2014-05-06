rm(list = ls())
# Use prepare.R to load data and function "forp"
a <- forp("PLN", "EUR", 1)
PPLNUSD <- a$data$p
# The following function will estimate the mixed model.  There are two regimes
# This works with PHUFEUR.  Now try PPLNUSD
require(depmixS4)
mod <- depmix(PPLNUSD ~ 1, data = da, ns = 2, 
              trst = c(0.9, 0.1, 0, 1), inst = c(1,0))
set.seed(1)
fm <- fit(mod, verbose = FALSE)
summary(fm)
# It looks like the higher return also has higher standard deviation. 
# However, this just switches once and does not switch back.  Need to look 
# at the model that will switch back and forth (stock market. )
pst <- posterior(fm)
head(pst)
tail(pst)
pst[,1]

#-------------------------------
library(TTR)
library(xts)
# load SP500 returns
plot(PPLNUSD, main = "PLN carry log returns", type = 'l')
# The aim is to identify the bull and bear markets. 
# First set up the model (mod). This is a model 
# of the log return with two states. Then fit the model.   
mod <- depmix(PPLNUSD ~ 1, nstates = 3, data = da)
 set.seed(2)
fm2 <- fit(mod, verbose = FALSE)
depmixS4::summary(fm2)
# state 1 Build carry
# State 2 Crash
# State 3 Caution
# plot
tPPLNUSD <- as.ts(PPLNUSD, start = c(2000, 01), frequency = 12)
posterior(fm2)[,1]
# What dates are system 2?
da$DATE[which(posterior(fm2)[,1] == 2)]
pbear <- as.ts(posterior(fm2)[, 2])
# This sets the bear market 
tsp(pbear) <- tsp(tPPLNUSD)
plot(cbind(tPPLNUSD, pbear), main = 
       "Posterior probability of state 2 (volatile, negative markets).")
# This does not work because of the time series properties of the fx data that
# is daily.  Would be nice to have the dates.  
# states
mapbear <- as.ts(posterior(fm2)[, 1] == 2)
tsp(mapbear) <- tsp(tPPLNUSD)
plot(cbind(tPPLNUSD, mapbear), 
     main = "Maximum a posteriori state sequence")
fm2@response
pst <- posterior(fm2)
head(pst)
tail(pst)
plot(pst[,2], type = 'l', main = "Probability in State 1: Build")
plot(pst[,3], type = 'l', main = "Probability in State 2: Crash")
plot(pst[,4], type = 'l', main = "Probability in State 3: Caution")
fm2@response
# It would be good to have dates to look at the times that the different 
# regimes were in place.  It would be good to check the relationship between
# Length of time in state 1 and the size of the crash.  This can be a 
# hypothesis
pdf("Figures/Test", paper= "a4", title = "PLN-USD Carry")
par(mfrow = c(4,1))
plot(PPLNUSD, main = "PLN carry log returns", type = 'l')
abline(h = 0)
plot(pst[,2], type = 'l', main = "Probability in State 1: Build")
plot(pst[,3], type = 'l', main = "Probability in State 2: Crash")
plot(pst[,4], type = 'l', main = "Probability in State 3: Caution")
dev.off()
