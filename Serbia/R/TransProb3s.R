# This will plot the effect of exogenous variables on the probability fo transforming 
# from position of clam to one of criss.  Need to create a table.
# rm(list = ls())
library(depmixS4) 
library(xtable)
set.seed(3)
# eventuaAICy want to create a function compare_model <- function(inv, )
inv <- c("AL", "BA", "GR", "HR",  "RS", "RU", 
         "SI", "TR", "UA")
# Next line may be useful for the table
# complist2 <- matrix(NA, nrow = length(inv), ncol = length(col), 
#                   dimnames = list(inv, col))
#comptab2 <- xtable(complist2, caption = "US rate model table", label = 
#      "tabref:comptab", digits = c(0, 2, 2, 2, 2, 2, 4, 2, 4, 2, 4, 2, 4))
#for(i in inv){
i = "RS"
col <- c("-3sd", "-2sd", "-1sd", "Mean", "+1sd","+2sd", "+3sd")
complist2 <- matrix(NA, nrow = length(inv), ncol = length(col), 
                   dimnames = list(inv, col))
# = "NOK"
# add the x values for the computation of the exogenous shock
x <- seq(-3, 3, 1)
for(i in inv){
  tempfile <- forp(i, "US", 1)
  tempfx <- tempfile$data$p
mod <- depmix(tempfx ~ 1, nstates = 3, transition = ~ scale(VIX), 
               data = da)               
fm <- fit(mod, verbose = TRUE, emcontrol = em.control(maxit = 1000))
fm@response[[1]][[1]]@parameters[[1]]
# identify the level order to rank the regimes.
levelvec <- c(fm@response[[1]][[1]]@parameters[[1]], 
              fm@response[[2]][[1]]@parameters[[1]],
              fm@response[[3]][[1]]@parameters[[1]])
selection <- rank(levelvec)[3]
selection2 <- rank(levelvec)[1]
if(selection == 2 && selection2 ==1){
  a <- fm@transition[[selection]]@parameters[[1]][3]
  b <- fm@transition[[selection]]@parameters[[1]][4]
  aa <- fm@transition[[selection]]@parameters[[1]][5]
  bb <- fm@transition[[selection]]@parameters[[1]][6]
ya <- a + b*x
yaa <- aa + bb*x
for(j in seq(1, 7, 1)){
  complist2[i, j] <- 1 - round( (1/(1+exp(-ya[j]))) + 
                                  (1/(1+exp(-yaa[j]))), 4)
}
} else  {
if(selection == 2 && selection2 == 3)
  a <- fm@transition[[selection]]@parameters[[1]][5]
  b <- fm@transition[[selection]]@parameters[[1]][6]
  ya <- a + b*x
for(j in seq(1, 7, 1)){
  complist2[i, j] <- round((1/(1+exp(-ya[j]))), 4)
}
} else {
if(selection == 1 && selection2 == 2)
  a <- fm@transition[[selection]]@parameters[[1]][3]
  b <- fm@transition[[selection]]@parameters[[1]][4]
  ya <- a + b*x
for(j in seq(1, 7, 1)){
  complist2[i, j] <- round((1/(1+exp(-ya[j]))), 4)
}
} else {
if(selection == 1 && selection2 == 3)
  a <- fm@transition[[selection]]@parameters[[1]][5]
  b <- fm@transition[[selection]]@parameters[[1]][6]
  ya <- a + b*x
for(j in seq(1, 7, 1)){
  complist2[i, j] <- round((1/(1+exp(-ya[j]))), 4)
}
} else {    
if(selection == 1 && selection2 == 3)
  a <- fm@transition[[selection]]@parameters[[1]][5]
  b <- fm@transition[[selection]]@parameters[[1]][6]
  ya <- a + b*x
for(j in seq(1, 7, 1)){
  complist2[i, j] <- round((1/(1+exp(-ya[j]))), 4)
}
} else { 
if(selection == 3 && selection == 1)
  a <- fm@transition[[selection]]@parameters[[1]][3]
  b <- fm@transition[[selection]]@parameters[[1]][4]
  aa <- fm@transition[[selection]]@parameters[[1]][5]
  bb <- fm@transition[[selection]]@parameters[[1]][6]
ya <- a + b*x
yaa <- aa + bb*x
for(j in seq(1, 7, 1)){
  complist2[i, j] <- 1 - round( (1/(1+exp(-ya[j]))) + 
                                  (1/(1+exp(-yaa[j]))), 4)
}
} else {
if(selection == 3 && selection2 == 2)
  a <- fm@transition[[selection]]@parameters[[1]][3]
  b <- fm@transition[[selection]]@parameters[[1]][4]
  ya <- a + b*x
for(j in seq(1, 7, 1)){
  complist2[i, j] <- round((1/(1+exp(-ya[j]))), 4)
}
}
}
complist2
write.csv(complist2, './Serbia/Data/ThreeRegCondSwitch.csv')
summary(fm)
fm
#---------------------------------------
tempfile <- forp(i, "US", 1)
tempfx <- tempfile$data$p
mod3 <- depmix(tempfx ~ 1, nstates = 2, data = da, transition = ~ scale(VIX))
# mod3 <- depmix(tempfx ~ 1, nstates = 3, data = da, transition = ~ scale(VIX))
fm3 <- fit(mod3, verbose = TRUE, emcontrol = em.control(maxit = 1000))# This will calculate the probability of switch conditional on deviation of 
# deviation of the exogenous from its mean
fm3@transition[[1]]@x
a <- fm3@transition[[2]]@parameters[[1]][3]
b <- fm3@transition[[2]]@parameters[[1]][4]
x <- seq(-3, 3, 0.1)
ya <- a + b*x
y <- 1/(1+exp(-ya))
exp
plot(x, y, type = 'l', xlab = "Standard deviations from mean", 
     ylab = "Probability", main = "Transition Probabililty")
summary(fm3)

