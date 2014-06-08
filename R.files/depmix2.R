# depmix vignette.  This is a run through to understand more about how things 
# work.  The maim aim is to find the way to extrct the parameters of the fitted 
# model
library("depmixS4")
data("speed")
set.seed(1)
mod <- depmix(response = rt ~ 1, data = "speed", nstates = 2, trstart = runif(4))
dim(speed)

