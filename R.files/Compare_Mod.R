# This will compare the likelihoods of the models.
# rm(list = ls())
# Use prepare2.R to load data and function "forp"
library(depmixS4) 
library(xtable)
set.seed(3)
inv <- c("HUF", "PLN", "CZK", "RON",  "RUB", "BGN", 
         "NOK", "ISK", "UAH", "HRK", "TRY")
col <- c("AIC1", "BIC1", "AIC2", "BIC2", "LR21", "LR21p", 
                        "AIC3", "BIC3", "LR31", "LR31p", "LR32", "LR32p")
complist <- matrix(1, nrow = 11, ncol = 12, dimnames = list(inv, col))
rownames(complist) <- inv
i = "TRY"
# for(i in inv){
tempfile <- forp(i, "EUR", 1)
tempfx <- tempfile$data$p
mod1 <- depmix(tempfx ~ 1, nstates = 1, data = da)
mod2 <- depmix(tempfx ~ 1, nstates = 2, data = da, transition = ~ scale(VIX))               
mod3 <- depmix(tempfx ~ 1, nstates = 3, data = da, transition = ~ scale(VIX))
fm1 <- fit(mod1, verbose = TRUE)
fm2 <- fit(mod2, verbose = TRUE, emcontrol = em.control(maxit = 1000))
fm3 <- fit(mod3, verbose = TRUE, emcontrol = em.control(maxit = 1000))
complist[i, 1] <- round(AIC(fm1), 2)
complist[i, 2] <- round(BIC(fm1), 2)
complist[i, 3] <- round(AIC(fm2), 2)
complist[i, 4] <- round(BIC(fm2), 2)
complist[i, 5] <- round(llratio(fm2, fm1)@value, 2)
complist[i, 6] <- round(1-pchisq(llratio(fm2, fm1)@value, llratio(fm2, fm1)@df), 4)
complist[i, 7] <- round(AIC(fm3), 2)
complist[i, 8] <- round(BIC(fm3), 2)
complist[i, 9] <- round(llratio(fm3, fm1)@value, 2)
complist[i, 10] <- round(1-pchisq(llratio(fm3, fm1)@value, llratio(fm3, fm1)@df), 4)
complist[i, 11] <- round(llratio(fm3, fm2)@value, 2)
complist[i, 12] <- round(1-pchisq(llratio(fm3, fm2)@value, llratio(fm3, fm2)@df), 4)
#}
complist
-----------------
summary(fm2)
llratio(fm3, fm1)

comptab2 <- xtable(complist, caption = "Comparison of models table", label = 
                    "tabref:comptab", align = c("l", c, c, c, c, c, c, c, c, c, c, c, c),
                  digits = c(2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 4, 2, 4))
comptab2
summary(fm2)
