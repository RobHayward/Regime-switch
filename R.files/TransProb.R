require(depmixS4)
set.seed(3)
# EEK and LVL do not seem to work. Is it the NAs? 
inv <- c("HUF", "PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
table <- matrix(NA, nrow = 2, ncol = length(inv))
rownames(table) <- c("Crisis Prob", "Calm Prob")
colnames(table) <- inv
j <- 'EUR'
# create a matrix for the parameters of the two models
for(i in inv){
  # EUR 1 month
  a <- forp(i, j, 1)
  profit <- a$data$p
  mod2 <- depmix(profit ~ 1, nstates = 2, data = da)
  fm2 <- fit(mod2, verbose = FALSE)
 #-------------
if(getpars(fm2)[7] > getpars(fm2)[9]){
  table[1, i] <- getpars(fm2)[4]
  table[2, i] <- 1 - getpars(fm2)[5]
} else {
  table[1, i] <- getpars(fm2)[5]
  table[2, i] <- 1 - getpars(fm2)[4]
}
}