rm(list = ls())
# Use prepare.R to load data and function "forp"
a <- forp("PLN", "EUR", 1)
PPLNUSD <- a$data$p
# The following function will estimate the mixed model.  There are two regimes
# This works with PHUFEUR.  Now try PPLNUSD
require(depmixS4)
#-------------------------------
library(TTR)
library(xts)
plot(PPLNUSD, main = "PLN carry log returns", type = 'l')
# The aim is to identify the bull and bear markets. 
# First set up the model (mod). This is a model 
# of the log return with two states. Then fit the model.   
mod <- depmix(PPLNUSD ~ 1, nstates = 3, data = da)
set.seed(3)
fm2 <- fit(mod, verbose = FALSE)
depmixS4::summary(fm2)
# state 1 Build carry
# State 2 Crash
# State 3 Caution
# plot
# What dates are system 2?
da$DATE[which(posterior(fm2)[,1] == 2)]
fm2@response
pst <- posterior(fm2)
--------------------------------
  # The following line will save a pdf in Figures for use
# pdf("Figures/PLNUSD.pdf", paper= "a4", title = "PLN-USD Carry")
par(mfrow = c(4,1))
# mfrow = c(4,1)) does not work.  Why? 
pst$Date <- index(PPLNUSD)
# This just adds a date series for the ppi file (the same as PPLNUSD)
plot(PPLNUSD, main = "PLN carry log returns", type = 'l')
abline(h = 1)
plot(pst[,2] ~ pst$Date, type = 'l', main = "Probability in State 1: Build")
plot(pst[,3] ~ pst$Date, type = 'l', main = "Probability in State 2: Crash")
plot(pst[,4] ~ pst$Date, type = 'l', main = "Probability in State 3: Caution")
# Use this line if pdf if used. 
# dev.off()

