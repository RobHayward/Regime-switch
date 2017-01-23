fm@response[[1]][[1]]@parameters[[1]]
levelvec <- c(fm@response[[1]][[1]]@parameters[[1]], 
              fm@response[[2]][[1]]@parameters[[1]],
              fm@response[[3]][[1]]@parameters[[1]])
selection <- rank(levelvec)[2]
a <- fm@transition[[selection]]@parameters[[1]][3]
b <- fm@transition[[selection]]@parameters[[1]][4]
a
b
fm@transition[[2]]@parameters
fm@transition
