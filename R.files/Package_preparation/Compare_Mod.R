# This will compare the AIC and the likelihoods of the models.
# rm(list = ls())
# Use prepare2.R to load data and function "forp"
# or source("R.files/prepare2.R")
# this is a table in teh file.
library(depmixS4) 
library(xtable)
set.seed(3)
# eventuaAICy want to create a function compare_model <- function(inv, )
inv <- c("HUF", "PLN", "CZK", "RON",  "RUB", "BGN", 
         "NOK", "ISK", "UAH", "HRK", "TRY")
col <- c("AIC1", "AIC2", "AIC3", "AIC4", "LR21",  "LR21p", "LR31", 
         "LR31p", "LR41", "LR41p", "LR43", "LR43p")
# this is added to the line above for 3 stage model.
#"AIC3", "BIC3", "LR31", "LR31p", "LR32", "LR32p")
complist2 <- matrix(NA, nrow = length(inv), ncol = length(col), 
                   dimnames = list(inv, col))
for(i in inv){
# i = "TRY"
tempfile <- forp(i, "EUR", 1)
tempfx <- tempfile$data$p
mod1 <- depmix(tempfx ~ 1, nstates = 1, data = da)
mod2 <- depmix(tempfx ~ VIX, nstates = 1, data = da)
mod3 <- depmix(tempfx ~ 1, nstates = 2, data = da)
mod4 <- depmix(tempfx ~ 1, nstates = 3, data = da)
# mod4 <- depmix(tempfx ~ 1, nstates = 2, data = da, transition = ~ scale(VIX))               
#mod3 <- depmix(tempfx ~ USD1MD, nstates = 3, data = da, transition = ~ scale(VIX))
# mod3 <- depmix(tempfx ~ 1, nstates = 3, data = da, transition = ~ scale(VIX))
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
#---------
complist2
comptab2 <- xtable(complist2, caption = "US rate model table", label = 
                     "tabref:comptab", digits = c(0, 2, 2, 2, 2, 2, 4, 2, 4, 2, 4, 2, 4))
comptab2
-----------------
  # This will not now work.  This is the transiation matrix when used.
  # Not even sure what I was trying to do here.  
fm3@transition[[1]]@x
a <- fm3@transition[[2]]@parameters[[1]][3]
b <- fm3@transition[[2]]@parameters[[1]][4]
x <- seq(-3, 3, 0.1)
ya <- a + b*x
y <- 1/(1+exp(-ya))
exp
plot(x, y, type = 'l', xlab = "Standard deviations from mean", 
     ylab = "Probability", main = "Transition Probabililty")
llratio(fm3, fm2)
comptab2
summary(fm2)
