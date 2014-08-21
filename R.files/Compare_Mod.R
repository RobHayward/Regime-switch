# This will compare the likelihoods of the models.
# rm(list = ls())
# Use prepare2.R to load data and function "forp"
library(depmixS4) 
library(xtable)
set.seed(3)
inv <- c("HUF", "PLN", "CZK", "RON",  "RUB", "BGN", 
         "NOK", "ISK", "UAH", "HRK", "TRY")
col <- c("AIC1", "ACI2", "AIC3", "LR21", "LR21p", "LR31", "LR31p", 
         "LR32", "LR32p")
# this is added to the line above for 3 stage model.
#"AIC3", "BIC3", "LR31", "LR31p", "LR32", "LR32p")
complist <- matrix(NA, nrow = length(inv), ncol = length(col), 
                   dimnames = list(inv, col))
rownames(complist) <- inv
#i = "TRY"
for(i in INV){
tempfile <- forp(i, "EUR", 1)
tempfx <- tempfile$data$p
mod1 <- depmix(tempfx ~ 1, nstates = 1, data = da)
mod2 <- depmix(tempfx ~ 1, nstates = 2, data = da)
mod3 <- depmix(tempfx ~ 1, nstates = 2, transition = ~ scale(USD1MD), 
               data = da)               
#mod3 <- depmix(tempfx ~ USD1MD, nstates = 3, data = da, transition = ~ scale(VIX))
# mod3 <- depmix(tempfx ~ 1, nstates = 3, data = da, transition = ~ scale(VIX))
fm1 <- fit(mod1, verbose = TRUE)
fm2 <- fit(mod2, verbose = TRUE, emcontrol = em.control(maxit = 1000))
fm3 <- fit(mod3, verbose = TRUE, emcontrol = em.control(maxit = 1000))
#reg <- lm(tempfx ~ USD1MD, data = da)
complist[i, 1] <- round(AIC(fm1), 2)
complist[i, 2] <- round(AIC(fm2), 2)
complist[i, 3] <- round(AIC(fm3), 2)
#complist[i, 4] <- round(BIC(fm2), 2)
complist[i, 4] <- round(llratio(fm2, fm1)@value, 2)
complist[i, 5] <- round(1-pchisq(llratio(fm2, fm1)@value, llratio(fm2, fm1)@df), 4)
#complist[i, 7] <- round(AIC(fm3), 2)
#complist[i, 8] <- round(BIC(fm3), 2)
complist[i, 6] <- round(llratio(fm3, fm1)@value, 1)
complist[i, 7] <- round(1-pchisq(llratio(fm3, fm1)@value, llratio(fm3, fm1)@df), 4)
complist[i, 8] <- round(llratio(fm3, fm2)@value, 1)
complist[i, 9] <- round(1-pchisq(llratio(fm3, fm2)@value, llratio(fm3, fm2)@df), 4)
# complist[i, 11] <- round(llratio(fm3, fm2)@value, 1)
# complist[i, 12] <- round(1-pchisq(llratio(fm3, fm2)@value, llratio(fm3, fm2)@df), 4)
}
complist


-----------------
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
comptab2 <- xtable(complist, caption = "US rate model table", label = 
                    "tabref:comptab", digits = c(2, 2, 2, 2, 2, 4, 2, 4, 4, 4))
comptab2
summary(fm2)
