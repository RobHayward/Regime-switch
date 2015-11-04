# this will look at the probability of a crisis at specific dates
# Idea is to see if the model can predict crisis
# Needs probabiolity of crisis at date and an average probability of crisis
require(depmixS4)
source("R.files/Package_preparation/FW.R")
CrisProb <- function(inv, fund, period = 1, number = 2, start.date = 
                       "2000-01-01", end.date = "2013-12-31"){
  a <- forp(inv, fund, period)
  a$data <- window(a$data, start = start.date, end = end.date)
  mod <- depmix(a$data$p ~ 1, nstates = number, data = a$data)
  fm <- fit(mod, verbose = TRUE, emcontrol = em.control(maxit = 1000))
  #trans <- getpars(fm)[1]
  return(fm)
}
b <- CrisProb("HUF", "EUR", end.date = "2005-12-31")
# I think that getpars(b)[4] is the probability of crash
getpars(b)[10]
vec <- getpars(b)[1:10]
mat1 <- matrix(vec[3:6], nrow = 2, byrow = TRUE)
mat1
mat2 <- matrix(vec[7:10], nrow = 2, byrow = FALSE)
mat2
# these give the parameters but which is the crisis and which is the calm? 
# Need the sorting function that is in the file. 
# Want to do this and then just take that part of a that runs up to the 
# appropriate date
# This is a zoo object. 
# Therefore can use
window(a$data, start = as.Date("2000-01-01"), end = as.Date("2005-12-31"))
window(a$data$p, start = as.Date("2000-01-01"), end = as.Date("2000-03-31"))
