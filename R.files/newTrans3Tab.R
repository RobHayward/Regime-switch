#states <- matrix(1:6, nrow = 2)
# states
#trans <- matrix(1:9, nrow = 3, byrow = TRUE)
# trans
# transtab <- matrix(1, nrow = 3, ncol = 3)
# statesvec <- matrix(1, nrow = 2, ncol = 3)
transmatrixlist <- list(c(inv))
for(i in inv){
states <- matrix(list4$EUR[[i]]$pars[13:18], nrow = 2)
states <- matrix(list4$EUR[[i]]$pars[13:18], nrow = 2)
trans <- matrix(list4$EUR[[i]]$pars[4:12], nrow = 3, byrow = TRUE)
states
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
transmatrixlist[[i]] <- list(states = round(states2, digits = 4), trans = 
                               round(trans3, digits = 4))
}
str(transmatrixlist)

# Inva - Take out "HUF".  There must be a more elegant way. 
inva <- c("PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
transformtable <- transmatrixlist[["HUF"]]$trans
for(i in inva){
transformtable <-  rbind(transformtable, transmatrixlist[[i]]$trans) 
}
transformtable
xtable(transformtable)
#-----------------------------------------