require(depmixS4)
set.seed(3)
inv <- c("HUF", "PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
table <- matrix(1, nrow = 4, ncol = length(inv))
rownames(table) <- c("mean1", "SD1", "mean2", "sd2") 
colnames(table) <- inv
list2 <- list(c(inv))
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
regimetable <- xtable(table2, digits = 4)
regimetable
table2
list <- list(c(inv))
str(list)
for(i in inv){
  x1 <- rnorm(10000, mean = table2[1, i], sd = table2[2,i])
  x2 <- rnorm(10000, mean = table2[3, i], sd = table2[4,i])
  x3 <- sample(na.omit(coredata(a$data$p)), size = 10000, 
               replace = TRUE)
list[[i]] <- list(Calm = x1, Crash = x2, Actual = x3)
}
# select rate PLN etc
x1 <- list$PLN$Calm
x2 <- list$PLN$Crash
x3 <- list$PLN$Actual

par(mfrow = c(3,1))
  hist(x1, prob = TRUE, breaks = 10, xlim = c(0.50, 1.4))
  lines(density(x1), col = 'red')
  hist(x2, prob = TRUE, breaks = 10, xlim = c(0.50, 1.4))
  lines(density(x2), col = 'red')
  hist(x3, prob = TRUE, breaks = 10, xlim = c(0.50, 1.4))
  lines(density(x3), col = 'red')
  # hist(x4, prob = TRUE, breaks = 10, xlim = c(0.50, 1.4))
  # lines(density(x4), col = 'red')
  # Or one file. 
  par(mfrow = c(1,1))
  plot(density(x2), col = 'blue', ylim = c(0, 12.0), 
       main = "Density of Caution, Build and Crash", 
       xlim = c(0.6, 1.4))
  lines(density(x1), col = 'red')
  lines(density(x3))
legend(0.65, 10, c("Calm", "Crash", "Total"), lty = c(1, 1, 1), 
       col = c("red", "blue", "black")) 
}
---------------------------------------------------------
# Create the probability charts
# First add the date index to list2 posterior.  
i = "TRY"
# Change this for 1 month EUR funded carry
title <- paste("Profits from ", i, "-", "EUR", "1M", sep= "")
# pdf("Figures/PLNUSD.pdf", paper= "a4", title = "PLN-USD Carry")
par(mfcol = c(3,2))
plot(list2[[i]]$profit, main = title, ylab = "Profits", xlab = "Date")
abline(h = 1)
plot(list2[[i]]$posterior$S1, type = 'l', main = "Probability of State Calm", 
     ylab = "Probability", xlab = "Date")
plot(list2[[i]]$posterior$S2, type = 'l', main = "Probability of State Crash", 
     ylab = "Probability", xlab = "Date")
# The following line will save a pdf in Figures for use
# dev.off()

# Get the date index
# This just adds a date series for the ppi file (the same as PPLNUSD)

