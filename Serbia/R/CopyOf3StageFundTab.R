# rm(list = ls())
require(depmixS4)
set.seed(3)
# these are the countries that seem to suggest a three stage model
inv <- c("BA", "HR", "RS", "RU", "SI", "TR", "UA")
fund <- c("US")
# I do not think that the next line is needed
#Funding <- list(c(fund))
table <- matrix(1, nrow = 6, ncol = length(inv))
rownames(table) <- c("mean1", "sd1", "mean2", "sd2", 
                     "mean3", "sd3") 
colnames(table) <- inv
# the list3 will hold the 3-stage results
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
# table 3 will order the state by size of the return (highest first for "calm")
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
# WIll write the informaton to a file called M3S
#write.csv(table3, './Serbia/Data/M3S.csv')
