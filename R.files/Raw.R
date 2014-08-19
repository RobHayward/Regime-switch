# rm(list = ls())
# Use prepare2.R to load data and function "forp"
a <- forp("PLN", "EUR", 1)
PPLNEUR <- a$data$p
# The following function will estimate the mixed model.  There are two regimes
# This works with PHUFEUR.  Now try PPLNUSD
require(depmixS4)
#-------------------------------
library(TTR)
library(xts)
# The aim is to identify the bull and bear markets. 
# First set up the model (mod). This is a model 
# of the log return with two states. Then fit the model.   
# scale
da$VIX <- scale(da$VIX)
set.seed(3)
mod1 <- depmix(PPLNEUR ~ 1, nstates = 1, data = da)
mod2 <- depmix(PPLNEUR ~ 1, nstates = 2, data = da, family = gaussian(), 
               transition = ~ scale(VIX))               
mod3 <- depmix(PPLNEUR ~ 1, nstates = 3, data = da, family = gaussian(), 
               transition = ~ scale(VIX))
# it is possible to set the staring values at this point. 
# trstart = transition start.
# instart = prior probabilities
# respstart = paramters of the response model. This is the most likely
# to be useful here from the VIX model in the doctorate. 
fm1 <- fit(mod1, verbose = TRUE)
fm2 <- fit(mod2, verbose = TRUE)
fm3 <- fit(mod3, verbose = TRUE)
fm1
fm2
fm3
depmixS4::summary(fm1)
depmixS4::summary(fm2)
depmixS4::summary(fm3)
llratio(fm2, fm3)
# state 1 Build carry
# State 2 Crash
# State 3 Caution
# plot
# What dates are system 2?
# Change state to crash
da$Date[which(posterior(fm2)[,1] == 2)]
da$Date[which(posterior(fm3)[,1] == 1)]
getpars(fm2)
getpars(fm3)[13]
pst2 <- posterior(fm2)
pst3 <- posterior(fm3)
--------------------------------
  # The following line will save a pdf in Figures for use
# pdf("Figures/PLNUSD.pdf", paper= "a4", title = "PLN-USD Carry")
par(mfrow = c(4,1))
# mfrow = c(4,1)) does not work.  Why? 
pst2$Date <- index(PPLNEUR)
pst3$Date <- index(PPLNEUR)
# This just adds a date series for the ppi file (the same as PPLNUSD)
plot(PPLNEUR, main = "PLN carry log returns", type = 'l')
abline(h = 1)
plot(pst3[,2] ~ pst2$Date, type = 'l', main = "Probability in State 1: Build")
plot(pst3[,3] ~ pst2$Date, type = 'l', main = "Probability in State 2: Crash")
plot(pst3[,4] ~ pst2$Date, type = 'l', main = "Probability in State 3: Caution")
# Use this line if pdf if used. 
# dev.off()
# This all set up to run through the currency paris to 
# extract the results. 
# Test rank

