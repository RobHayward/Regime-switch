# Extract the transition matrix and tabulate.
# This works for one.  Now need to loop
i = "PLN"
#for(i in Inv){
# use list4 for 3 stage
list2[[i]]["pars"]
#list4$EUR$PLN$pars
states <- matrix(list2$HUF$pars[7:10], nrow = 2)
trans <- matrix(list2$HUF$pars[3:6], nrow = 2, byrow = TRUE)
#states <- matrix(1:6, nrow = 2)
states
#trans <- matrix(1:9, nrow = 3, byrow = TRUE)
trans
# transtab <- matrix(1, nrow = 3, ncol = 3)
# statesvec <- matrix(1, nrow = 2, ncol = 3)
states1 <- states[, rank(states[1,])]
states1
states2 <- states1[,c(2, 3, 1)]
states2
trans1 <- trans[,rank(states[1,])]
trans1
trans2 <- trans1[rank(states[1,]),]
trans2
trans3 <- trans2[, c(2, 3, 1)]
trans3 <- trans3[c(2, 3, 1),]
trans3
colnames(states2) <- c("Caution", "Build", "Crash")
rownames(states2) <- c("Mean", "Standard-Deviation")
colnames(trans3) <- c("To Caution", "To Build", "To Crash")
rownames(trans3) <- c("From Caution", "From Build", "From Crash")
round(states2, digits = 4)
round(trans3, digits = 4)

# I do not think that the rest is needed.  Check.  

# This works for levels, but not for matrix. 
# for(i in inv){
  if(states[1, 1] < states[1, 2] & states[1, 1] < states[1, 3]) {
    statesvec[1,3] <- states[1, 1] 
    statesvec[2,3] <- states[2, 1]
 # a => c
    transtab[3, 3] <- trans[1, 1]
    transtab[1, 2] <- trans[1, 2]
    transtab[]
  } else if(states[1, 1] >  states[1, 2] & states[1, 2] > states[1, 3]) {
    statesvec[1,2] <- states[1, 1]
    statesvec[2,2] <- states[2, 1]
  # a => b
    transtab[2, 2] <- trans[1, 1] 
  } else  {
    statesvec[1, 1] <- states[1, 1]
    statesvec[2, 1] <- states[2, 1]
  # a = a
    transtab[1, 1] <- trans[1, 1] }
  
  if(states[1, 3] < states[1, 1] & states[1, 3] < states[1, 2]) {
    statesvec[1, 3] <- states[1, 3]
    statesvec[2, 3] <- states[2, 3]
  # c = c  
    transtab[, 3] <- trans[,3]
 } else if(states[1, 3] >  states[1, 1] & states[1, 3] > states[1, 2]) {
    statesvec[1, 2] <- states[1, 3]
    statesvec[2, 2] <- states[2, 3]
  # c => b  
    transtab[, 2] <- trans[,3]
 } else  {
    statesvec[1, 1] <- states[1, 3]
    statesvec[2, 1] <- states[2, 3]
  # c => a  
    transtab[, 1] <- trans[, 3] 
  }
  if(states[1, 2] < states[1, 1] & states[1, 2] < states[1, 3]) {
    statesvec[1, 3] <- states[1, 2]
    statesvec[2, 3] <- states[2, 2]
  # b => c
    transtab[, 3] <- trans[, 2]
  } else if(states[1, 2] >  states[1, 1] & states[1, 2] > states[1, 3]) {
    statesvec[1, 2] <- states[1, 2]
    statesvec[2, 2] <- states[2, 2]
  # b = b  
    transtab[, 2] <- trans[, 2] 
  } else  {
    statesvec[1, 1] <- states[1, 2]
    statesvec[2, 1] <- states[2, 2]
  # b => a
    transtab[, 1] <- trans[, 2] 
  }

rownames(transtab) <- c("From Caution", "From Build", "From Crash")
colnames(transtab) <- c("To Caution", "To Build", "To Crash")
rownames(statesvec) <- c("Mean Return", "Standard Deviation")
colnames(statesvec) <- c("Caution", "Build", "Crash")
round(transtab, digits = 4)
round(statesvec, digits = 4)
