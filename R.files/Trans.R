# Extract the transition matrix and tabulate.
# This works for one.  Now need to loop
list4$EUR$PLN$pars
states <- matrix(list4$EUR$PLN$pars[13:18], nrow = 2)
trans <- matrix(list4$EUR$PLN$pars[4:12], nrow = 3)
states
trans
list4$EUR$PLN$pars[1:18]
states

transtab <- matrix(1, nrow = 3, ncol = 3)
statesvec <- matrix(1, nrow = 2, ncol = 3)
# for(i in inv){
  if(states[1, 1] < states[1, 2] & states[1, 1] < states[1, 3]) {
    statesvec[1,3] <- states[1, 1] 
    statesvec[2,3] <- states[1, 2]
    transtab[, 3] <- trans[, 1]
  } else if(states[1, 1] >  states[1, 2] & states[1, 2] > states[1, 3]) {
    statesvec[1,2] <- states[1, 1]
    statesvec[2,2] <- states[1, 2]
    transtab[, 2] <- trans[, 1] 
  } else  {
    statesvec[1, 1] <- states[1, 1]
    statesvec[2, 1] <- states[1, 2]
    transtab[, 2] <- trans[, 1] }
  
  if(states[1, 3] < states[1, 1] & states[1, 3] < states[1, 2]) {
    statesvec[1, 3] <- states[1, 3]
    statesvec[2, 3] <- states[2, 3]
    transtab[, 3] <- trans[,3]
 } else if(states[1, 3] >  states[1, 1] & states[1, 3] > states[1, 2]) {
    statesvec[1, 2] <- states[1, 3]
    statesvec[2, 2] <- states[2, 3]
    transtab[, 2] <- trans[,3]
 } else  {
    statesvec[1, 1] <- states[1, 3]
    statesvec[1, 2] <- states[2, 3]
    transtab[, 1] <- trans[, 3] 
  }
  if(states[1, 2] < states[1, 1] & states[1, 2] < states[1, 3]) {
    statesvec[1, 3] <- states[1, 2]
    statesvec[2, 3] <- states[2, 2]
    transtab[, 3] <- trans[, 2]
  } else if(states[1, 2] >  states[1, 1] & states[1, 2] > states[1, 3]) {
    statesvec[1, 2] <- states[1, 2]
    statesvec[2, 2] <- states[2, 2]
    transtab[, 2] <- trans[, 2] 
  } else  {
    statesvec[1, 1] <- states[1, 2]
    statesvec[2, 1] <- states[2, 2]
    transtab[, 1] <- trans[, 2] 
  }

statsvec <- matrix(c(a1, a2, b1, b2, c1, c2, nrow = 3))
rownames(transtab) <- c("From Caution", "From Build", "From Crash")
colnames(transtab) <- c("To Caution", "To BUild", "To Crash")
rownames(statesvec) <- c("Mean Return", "Standard Deviation")
colnames(statesvec) <- c("Caution", "Build", "Crash")
round(transtab, digits = 4)
round(statesvec, digits = 4)
