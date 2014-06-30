require(depmixS4)
set.seed(3)
# EEK and LVL do not seem to work. Is it the NAs? 
inv <- c("HUF", "PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
fund <- c("EUR", "USD", "CHF", "JPY")
funding <- list(c(fund))
for(j in fund){
# create a matrix for the parameters of the two models
list2 <- list(c(inv))
table <- matrix(1, nrow = 4, ncol = length(inv))
rownames(table) <- c("mean1", "SD1", "mean2", "sd2") 
colnames(table) <- inv
for(i in inv){
  # EUR 1 month
  a <- forp(i, "EUR", 1)
  profit <- a$data$p
  mod2 <- depmix(profit ~ 1, nstates = 2, data = da)
  fm2 <- fit(mod2, verbose = FALSE)
  table[1,i] <- getpars(fm2)[7]
  table[2,i] <- getpars(fm2)[8]
  table[3,i] <- getpars(fm2)[9]
  table[4,i] <- getpars(fm2)[10]
  list2[[i]] <- list(pars = getpars(fm2), logLik(fm2), 
                     posterior = posterior(fm2), profit = a$profit)
  list2[[i]]$posterior$Date <- index(list2[[i]]$profit)
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
table3 <- rbind(funding$EUR, funding$USD, funding$CHF, funding$JPY)
regimetable <- xtable(table3, digits = 4)
regimetable
table2