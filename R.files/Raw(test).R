#rm(list = ls())
library(TTR)
library(xts)
require(xtable)
# This will test other exchange rates with the Raw file.  Then needs to be 
# automated. That is in the prepare.R file. 
# Use prepare.R to load data and function "forp"
a <- forp("HUF", "USD", 1)
# This can be improved.
PHUFUSD <- a$data$p
#-------------------------------
# The aim is to identify the caution, build and crash regimes. 
# First set up the model (mod). This is a model 
# of the log return with three states. Then fit the model.   
mod <- depmix(PHUFUSD ~ 1, nstates = 3, data = da)
 set.seed(2)
fm2 <- fit(mod, verbose = FALSE)
depmixS4::summary(fm2)
s1 <- fm2@response[[1]][1]
s1
s2 <- fm2@response[[2]][1]
s2
s3 <- fm2@response[[3]][1]

# state 1 Crash?
# State 2 Caution?
# State 3 Build
# plot
# tPHUFUSD <- as.ts(PPLNUSD, start = c(2000, 01), frequency = 12)
# posterior(fm2)[,1]
# What dates are system 2?
# This gives the dates of the required state
da$DATE[which(posterior(fm2)[,1] == 2)]
# fm2@response ## to get responses 
# pst <- posterior(fm2) # [posterios probs of states]
# head(pst)
# tail(pst)

pdf("Figures/HUFUSD", paper= "a4", title = "HUF-USD Carry")
par(mfrow = c(4,1))
pst$Date <- index(PHUFUSD)
# This just adds a date series for the ppi file (the same as PPLNUSD)
plot(PHUFUSD, main = "HUF carry", type = 'l')
abline(h = 1)
plot(pst[,2] ~ pst$Date, type = 'l', main = "Probability in State 1: Build")
plot(pst[,3] ~ pst$Date, type = 'l', main = "Probability in State 2: Crash")
plot(pst[,4] ~ pst$Date, type = 'l', main = "Probability in State 3: Caution")
dev.off()

