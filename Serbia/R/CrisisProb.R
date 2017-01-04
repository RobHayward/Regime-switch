# this will look at the probability of a crisis at specific dates
# Idea is to see if the model can predict crisis
# Needs probabiolity of crisis at date and an average probability of crisis
require(depmixS4)
source("R.files/Package_preparation/FW.R")
# add investment currency, funding currency, investment period, the number 
# of regimes, the start date and end date. 
CrisProb <- function(inv, fund, period = 1, reg.number = 2, start.date = 
                       "2000-01-01", end.date = "2013-12-31"){
  a <- forp(inv, fund, period)
  a$data <- window(a$data, start = start.date, end = end.date)
  mod <- depmix(a$data$p ~ 1, nstates = reg.number, data = a$data)
  fm <- fit(mod, verbose = TRUE, emcontrol = em.control(maxit = 1000))
  #trans <- getpars(fm)[1]
#k  return(fm)

# Remember to put the correct date into the function. 
# I think that getpars(b)[4] is the probability of crash
vec <- getpars(fm)[1:10]
if(vec[7] > vec[9]){
mat1 <- matrix(vec[7:10], nrow = 2, byrow = FALSE)
mat2 <- matrix(vec[3:6], nrow = 2, byrow = TRUE)
} else
  {
mat1 <- matrix(vec[c(9, 10, 7, 8)], nrow = 2, byrow = FALSE)
mat2 <- matrix(vec[c(6, 5, 3, 4)], nrow = 2, byrow = TRUE)
}
return(mat2[1, 2])
}


