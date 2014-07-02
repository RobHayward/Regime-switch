require(depmixS4)
set.seed(3)
# EEK and LVL do not seem to work. Is it the NAs? 
inv <- c("HUF", "PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
# create a matrix for the parameters of the two models
table <- matrix(1, nrow = 6, ncol = length(inv))
rownames(table) <- c("mean1", "sd1", "mean2", "sd2", 
                     "mean3", "sd3") 
colnames(table) <- inv
list3 <- list(c(inv))
for(i in inv){
  # EUR 1 month
  a <- forp(i, "EUR", 1)
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
table
table2 <- matrix(1, nrow = 6, ncol = length(inv))
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
str(list3)
table3
rowMeans(table3)
i = "HUF"
# -------------Create tables for funding currencies
regimetable <- xtable(table3, digits = 4)
regimetable
# Loop through funding currencies in 3StageFundTab.R 
-----------------------------------------------
list <- list(c(inv))
str(list)
for(i in inv){
  x1 <- rnorm(10000, mean = table3[1, i], sd = table3[2,i])
  x2 <- rnorm(10000, mean = table3[3, i], sd = table3[4,i])
  x3 <- rnorm(10000, mean = table3[5, i], sd = table3[4,i])
  x4 <- sample(na.omit(coredata(a$data$p)), size = 10000, 
               replace = TRUE)
list[[i]] <- list(Caution = x1, Build = x2, Crash = x3, All = x4)
}
str(list)
# select rate PLN etc to create histograms.
x1 <- list$HUF$Caution
x2 <- list$HUF$Build
x3 <- list$HUF$Crash
x4 <- list$HUF$All

par(mfrow = c(4,1))
# breaks <- seq(0.5, 1.4, 0.05)  
  hist(x1, prob = TRUE, xlim = c(0.50, 1.4))
  lines(density(x1), col = 'red')
  hist(x2, prob = TRUE, xlim = c(0.50, 1.4))
  lines(density(x2), col = 'red')
  hist(x3, prob = TRUE, xlim = c(0.50, 1.4))
  lines(density(x3), col = 'red')
  hist(x4, prob = TRUE, xlim = c(0.50, 1.4))
  lines(density(x4), col = 'red')

# hist(x4, prob = TRUE, breaks = 10, xlim = c(0.50, 1.4))
  # lines(density(x4), col = 'red')
  # Or one file. 
par(mfrow = c(1,1))
plot(density(x2), col = 'blue', ylim = c(0, 12.0), 
       main = "Density of Caution, Build and Crash", 
       xlim = c(0.6, 1.4))
lines(density(x1), col = 'red')
lines(density(x3), col = "green")
lines(density(x4))
legend(0.65, 10, c("Caution", "Build", "Crash", "All"), lty = c(1, 1, 1, 1), 
       col = c("red", "blue", "green", "black")) 
---------------------------------------------------------
# Create the probability charts
# First add the date index to list2 posterior.
list3$HUF$posterior$Date <- index(a$profit)  
i = "UAH"
# Change this for 1 month EUR funded carry
# This will have to be manually manipuated as S1 does not 
# equal caution. s2 does not equal build. 
title <- paste("Profits from ", i, "-", "EUR", "1M", sep= "")
pdf("Figures/3RegProb/HRKUAHEUR.pdf", paper= "a4", title = "HRK-EUR and 
    UAH-EUR Carry")
par(mfcol = c(4,2))
plot(list3[[i]]$profit, main = title, ylab = "Profits", xlab = "Date")
abline(h = 1)
plot(list3[[i]]$posterior$S1 ~ list3[[i]]$posterior$Date, 
     type = 'l', main = "Probability of State Caution", 
     ylab = "Probability", xlab = "Date")
plot(list3[[i]]$posterior$S3 ~ list3[[i]]$posterior$Date, type = 'l',  
     main = "Probability of State Build", ylab = "Probability", xlab = "Date")
plot(list3[[i]]$posterior$S2 ~ list3[[i]]$posterior$Date, type = 'l',  
     main = "Probability of State Crash", ylab = "Probability", xlab = "Date")
str(list3$HUF$posterior)
# The following line will save a pdf in Figures for use
dev.off()

