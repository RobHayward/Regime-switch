require(depmixS4)
set.seed(3)
# EEK and LVL do not seem to work. Is it the NAs? 
inv <- c("HUF", "PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
fund <- c("EUR", "USD", "CHF", "JPY")
funding <- list(c(fund))
list2 <- list(c(inv))
table <- matrix(1, nrow = 4, ncol = length(inv))
rownames(table) <- c("mean1", "SD1", "mean2", "sd2") 
colnames(table) <- inv
for(j in fund){
# create a matrix for the parameters of the two models
for(i in inv){
  # EUR 1 month
  a <- forp(i, j, 1)
  profit <- a$data$p
  mod2 <- depmix(profit ~ 1, nstates = 2, data = da)
  fm2 <- fit(mod2, verbose = FALSE)
  table[1,i] <- getpars(fm2)[7]
  table[2,i] <- getpars(fm2)[8]
  table[3,i] <- getpars(fm2)[9]
  table[4,i] <- getpars(fm2)[10]
  list2[[j]][[i]] <- list(pars = getpars(fm2), ll = logLik(fm2), 
                     AIC = AIC(fm2), BIC = BIC(fm2), posterior = posterior(fm2), 
                          profit = a$profit)
  list2[[j]][[i]]$posterior$Date <- index(list2[[j]][[i]]$profit)
}
# table 2 will order the state by size of the return (highest first for "calm")
table2 <- matrix(1, nrow = 4, ncol = length(inv))
rownames(table2) <- c("Calm", "SD1", "Crash", "sd2") 
colnames(table2) <- inv
for(i in inv){
  if(table[1, i] < table[3, i]){
    table2[1:2, i] <- table[3:4, i]
    table2[3:4, i] <- table[1:2, i]
  } 
  if(table[1, i] > table[3, i]){
    table2[1:2, i] <- table[1:2, i]
    table2[3:4, i] <- table[3:4, i]
  }
}
funding[[j]] <- table2
}
str(funding)
head(funding$EUR)
table2 <- rbind(funding$EUR, funding$USD, funding$CHF, funding$JPY)
table2 <- cbind(table2, rowMeans(table2))
table2
colnames(table2) <- c(inv, "Mean")
regimetable <- xtable(table2, digits = 4)
regimetable
table2
#------------------------------------------------------------------
# Now want to find average for fixed and floating exchange rates
# vector of currency regimes for inv.  1 is fixed, 2 is mixed, 3 is floating
# adapted from Hayward (2013) and  IMF (2009)
inv
fxregime <- c(2, 3, 3, 2, 2, 3, 1, 3, 3, 2, 2, 0)
table2 <- rbind(table2, fxregime)
meanfloat <- rowMeans(table2[,which(table2[17,] == 3)])
meanfix <- rowMeans(table2[,which(table2[17,] == 2)])
meanfloat - meanfix
#-------------
print(fm2)
AIC(fm2)
BIC(fm2)
logLik(fm2)
str(list2[["EUR"]][["PLN"]])
# create a table for the criteria
# This is being built for all the criteria that will determine the best model
critable2 <- matrix(1, nrow = length(inv), ncol = 3)
rownames(critable2) <- inv
colnames(critable2) <- c("LL", "AIC", "BIC")
for(i in inv){
  critable2[i, 1] <- list2[["EUR"]][[i]]$ll
  critable2[i, 2] <- list2[["EUR"]][[i]]$AIC
  critable2[i, 3] <- list2[["EUR"]][[i]]$BIC
}
critable2
---------------------------------------------------------