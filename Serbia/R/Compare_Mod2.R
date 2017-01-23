# This will compare the AIC and the likelihoods of the models.
# rm(list = ls())
library(depmixS4) 
library(xtable)
set.seed(3)
# there are two tables:  1 is Albania and Greece; 2 is the rest
inv1 <- c("AL", "GR")
inv2 <- c("BA", "HR",  "RS", "RU", "SI", "TR", "UA")
# there are two tables col1 tests Albania and Greece
#  M1 = basic
#  M2 = VIX slope
# M3 is 2-regime
# M4 is 2 regime with VIX
col1 <- c("AIC1", "AIC2", "AIC3", "AIC4", "LR21",  "LR21p", "LR31", 
         "LR31p", "LR41", "LR41p", "LR43", "LR43p")
# col2 tests others the models are 
#  M1 = basic
#  M2 = VIX slope
# M3 is 3-regime
# M4 is 3 regime with VIX
col2 <- c("AIC1", "AIC2", "AIC3", "AIC4", "LR21",  "LR21p", "LR31", 
         "LR31p", "LR41", "LR41p", "LR43", "LR43p")
# threre are two tables (1 and 2)
complist1 <- matrix(NA, nrow = length(inv1), ncol = length(col1), 
                   dimnames = list(inv1, col1))
complist2 <- matrix(NA, nrow = length(inv2), ncol = length(col2), 
                   dimnames = list(inv2, col2))
# mod3a and mod4a are de-selected for the fist list of countries
#mod3 and mod4 are de-selected for second list of countries
for(i in inv2){
# i = "TRY"
tempfile <- forp(i, "US", 1)
tempfx <- tempfile$data$p
mod1 <- depmix(tempfx ~ 1, nstates = 1, data = da)
mod2 <- depmix(tempfx ~ VIX, nstates = 1, data = da)
#mod3 <- depmix(tempfx ~ 1, nstates = 2, data = da)
mod3a <- depmix(tempfx ~ 1, nstates = 3, data = da)
#mod4 <- depmix(tempfx ~ VIX, nstates = 2, data = da, transition = ~ scale(VIX))
mod4a <- depmix(tempfx ~ VIX, nstates = 3, data = da, transition = ~ scale(VIX))
#------------------------------------
fm1 <- fit(mod1, verbose = TRUE)
fm2 <- fit(mod2, verbose = TRUE, emcontrol = em.control(maxit = 1000))
fm3 <- fit(mod3, verbose = TRUE, emcontrol = em.control(maxit = 1000))
# maximum interations incresed for the fm4
fm4 <- fit(mod4, verbose = TRUE, emcontrol = em.control(maxit = 10000))
complist2[i, 1] <- round(AIC(fm1), 2)
complist2[i, 2] <- round(AIC(fm2), 2)
complist2[i, 3] <- round(AIC(fm3), 2)
complist2[i, 4] <- round(AIC(fm4), 2)
complist2[i, 5] <- round(llratio(fm2, fm1)@value, 2)
complist2[i, 6] <- round(1-pchisq(llratio(fm2, fm1)@value, llratio(fm2, fm1)@df), 4)
complist2[i, 7] <- round(llratio(fm3, fm1)@value, 2)
complist2[i, 8] <- round(1-pchisq(llratio(fm3, fm1)@value, llratio(fm3, fm1)@df), 4)
complist2[i, 9] <- round(llratio(fm4, fm1)@value, 2)
complist2[i, 10] <- round(1-pchisq(llratio(fm4, fm1)@value, llratio(fm4, fm3)@df), 4)
complist2[i, 11] <- round(llratio(fm4, fm3)@value, 2)
complist2[i, 12] <- round(1-pchisq(llratio(fm4, fm3)@value, llratio(fm4, fm3)@df), 4)
}
complist2
write.csv(complist1, './Serbia/Data/CompTable2.csv')
write.csv(complist2, './Serbia/Data/CompTable3.csv')
#---------
comptab2 <- xtable(complist2, caption = "US rate model table", label = 
                     "tabref:comptab", digits = c(0, 2, 2, 2, 2, 2, 4, 2, 4, 2, 4, 2, 4))
comptab2
-----------------
  # This will calculate the probability of switch conditional on deviation of 
  # deviation of the exogenous from its mean
fm3@transition[[1]]@x
a <- fm3@transition[[2]]@parameters[[1]][3]
b <- fm3@transition[[2]]@parameters[[1]][4]
x <- seq(-3, 3, 0.1)
ya <- a + b*x
y <- 1/(1+exp(-ya))
exp
plot(x, y, type = 'l', xlab = "Standard deviations from mean", 
     ylab = "Probability", main = "Transition Probabililty")
llratioratio(fm3, fm2)
comptab2
summary(fm2)
