rm(list = ls())
# Use prepare.R to load data and function "forp"
a <- forp("PLN", "EUR", 1)
PPLNEUR <- a$data$p
library(depmixS4)
library(TTR)
library(xts)
mod <- depmix(PPLNUSD ~ 1, nstates = 3, data = da)
set.seed(2)
fm2 <- fit(mod, verbose = FALSE)
depmixS4::summary(fm2)
tPPLNEUR <- as.ts(PPLNUSD, start = c(2000, 1), frequency = 12)
str(tPPLNEUR)
posterior(fm2)[,1]
# What are the dates for crash mode?
da$Date[which(posterior(fm2)[,1] == 3)]
# This whole section does not work
# pbear <- as.ts(posterior(fm2)[, 1])
# plot(pbear)
# This 
# tsp(pbear) <- tsp(tPPLNUSD)
# plot(cbind(tPPLNUSD, pbear), main = 
#       "Posterior probability of state 2 (volatile, negative markets).")
# This does not work because of the time series properties of the fx data that
# is daily.  Would be nice to have the dates.  
# states
# mapbear <- as.ts(posterior(fm2)[, 1] == 2)
# tsp(mapbear) <- tsp(tPPLNUSD)
# plot(cbind(tPPLNUSD, mapbear), 
#     main = "Maximum a posteriori state sequence")
# fm2@response
# pst <- posterior(fm2)
# pdf("Figures/PLNUSD.pdf", paper= "a4", title = "PLN-USD Carry")
par(mfrow = c(4,1))
pst$Date <- index(PPLNEUR)
# This just adds a date series for the ppi file (the same as PPLNUSD)
plot(PPLNEUR, main = "PLN carry log returns", type = 'l')
abline(h = 1)
plot(pst[,2] ~ pst$Date, type = 'l', main = "Probability in State 1: Caution")
plot(pst[,3] ~ pst$Date, type = 'l', main = "Probability in State 2: Build")
plot(pst[,4] ~ pst$Date, type = 'l', main = "Probability in State 3: Crash")
dev.off()

