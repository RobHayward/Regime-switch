# Extract the transition matrix and tabulate.
# This works for one.  Now need to loop
#summary(fm)
#getpars(fm)
#trans2 <- matrix(getpars(fm)[3:6], ncol = 2, byrow = TRUE)
#trans2
list4$EUR$PLN$pars
states <- matrix(list4$EUR$TRY$pars[13:18], nrow = 2)
trans <- matrix(list4$EUR$TRY$pars[4:12], nrow = 3, byrow = TRUE)
states <- matrix(1:6, nrow = 2)
states
trans <- matrix(1:9, nrow = 3, byrow = TRUE)
trans
list4$EUR$PLN$pars[1:18]
states

transtab <- matrix(1, nrow = 3, ncol = 3)
statesvec <- matrix(1, nrow = 2, ncol = 3)
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
