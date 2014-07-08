# Extract the transition matrix and tabulate.
transition <- list(c(inv))
transition
states <- matrix(list4$EUR$HUF$pars[13:18], nrow = 2)
trans <- matrix(list4$EUR$HUF$pars[4:12], nrow = 3)
states
trans
list4$EUR$HUF$pars[1:18]

transtab <- matrix(1, nrow = 3, ncol = 3
rownames(transtab) <- c("From Cautious", "From Build", "From Crash") 
colnames(transtab) <- c("To Cautious", "To Build", "To Crash" )
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
