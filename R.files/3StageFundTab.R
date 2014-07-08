require(depmixS4)
set.seed(3)
# EEK and LVL do not seem to work. Is it the NAs? 
# RUB does not work with CHF
inv <- c("HUF", "PLN", "CZK", "RON", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
fund <- c("EUR", "USD", "CHF", "JPY")
funding <- list(c(fund))
list4 <- list(c(fund))
list4
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
  list3[[i]] <- list(pars = getpars(fm3), logLik(fm3), 
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
table3
funding[[j]] <- table3
list4[[j]] <- list3
}
str(funding)
# RUB does not work in CHF.  
# correct CHF for the removal of RUB. 
funding$JPY
a <- rep(NA, 6)
str(list4)
funding$CHF <- cbind(funding$CHF, rep(0, 6))
colnames(funding$CHF) <- inv
funding$CHF <- funding$CHF[,c(1,2,3, 4, 11, 5, 6, 7, 8, 9, 10)]
table4 <- rbind(funding$EUR, funding$USD, funding$CHF, funding$JPY)
rowMeans(table4)
regimetable <- xtable(table4, digits = 4)
regimetable
table3
m <- matrix(1:9, nrow = 3)
m[,2] <- c(1, 2, 3)
m
m <- cbind(m, c(1, 2, 4))
m <- m[,c(1, 4, 2, 3)]
m
