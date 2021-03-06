# rm(list = ls())
require(depmixS4)
set.seed(3)
# EEK and LVL do not seem to work. Is it the NAs? 
# RUB does not work with CHF
inv <- c("HUF", "PLN", "CZK", "RON", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
fund <- c("EUR", "USD", "CHF", "JPY")
funding <- list(c(fund))
list4 <- list(c(fund))
for(j in fund){
# create a matrix for the parameters of the two models
table <- matrix(1, nrow = 6, ncol = length(inv))
rownames(table) <- c("mean1", "sd1", "mean2", "sd2", 
                     "mean3", "sd3") 
colnames(table) <- inv
list3 <- list(c(inv))
for(i in inv){
  # EUR 1 month
  a <- forp(i, j, 1)
  profit <- a$data$p
  mod3 <- depmix(profit ~ 1, nstates = 3, data = da)
  fm3 <- fit(mod3, verbose = FALSE)
  # 3 initial conditions, 9 transitions, 6 return parameters
  table[1,i] <- getpars(fm3)[13]
  table[2,i] <- getpars(fm3)[14]
  table[3,i] <- getpars(fm3)[15]
  table[4,i] <- getpars(fm3)[16]
  table[5,i] <- getpars(fm3)[17]
  table[6,i] <- getpars(fm3)[18]
  list3[[i]] <- list(pars = getpars(fm3), LL = logLik(fm3), 
                     AIC = AIC(fm3), BIC = BIC(fm3),
                     posterior = posterior(fm3), profit = a$profit)
  list3[[i]]$posterior$Date <- index(list3[[i]]$profit)
}
# table 2 will order the state by size of the return (highest first for "calm")
table3 <- matrix(1, nrow = 6, ncol = length(inv))
rownames(table3) <- c("Calm", "SD1", "Build", "SD2", "Crash", "SD3") 
colnames(table3) <- inv
for(i in inv){
  if(table[1, i] < table[3, i] & table[1, i] < table[5, i]) {
    table3[5:6, i] <- table[1:2, i]
  } else if(table[1, i] >  table[3, i] & table[1, i] > table[5, i]) {
    table3[3:4, i] <- table[1:2, i] 
  } else  {
    table3[1:2, i] <- table[1:2, i] }
  
  if(table[3, i] < table[1, i] & table[3, i] < table[5, i]) {
    table3[5:6, i] <- table[3:4, i]
  } else if(table[3, i] >  table[1, i] & table[3, i] > table[5, i]) {
    table3[3:4, i] <- table[3:4, i] 
  } else  {
    table3[1:2, i] <- table[3:4, i] 
  }
  if(table[5, i] < table[1, i] & table[5, i] < table[3, i]) {
    table3[5:6, i] <- table[5:6, i]
  } else if(table[5, i] >  table[1, i] & table[5, i] > table[3, i]) {
    table3[3:4, i] <- table[5:6, i] 
  } else  {
    table3[1:2, i] <- table[5:6, i] 
  }
  
}

funding[[j]] <- table3
list4[[j]] <- list3
}
# Put tables together, calculate mean and assess.
str(funding)
# 
table4 <- rbind(funding$EUR, funding$USD, funding$CHF, funding$JPY)
table4 <- cbind(table4, rowMeans(table4))
table4
colnames(table4) <- c(inv, "Mean")
regimetable <- xtable(table4, digits = 4)
regimetable
# ------------------------
str(list4)
critable3 <- matrix(1, nrow = length(inv), ncol = 3)
rownames(critable3) <- inv
colnames(critable3) <- c("LL", "AIC", "BIC")
for(i in inv){
  critable3[i, 1] <- list4[["EUR"]][[i]]$LL
  critable3[i, 2] <- list4[["EUR"]][[i]]$AIC
  critable3[i, 3] <- list4[["EUR"]][[i]]$BIC
}
critable3
# Combine tables 2 and 3.  

inv