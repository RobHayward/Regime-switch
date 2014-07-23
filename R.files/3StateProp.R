# list3 contains pars for the parameters of the model, the logkelihood 
# the posterior, a dataframe that contains the most likely state, $state, 
# Use this pattern to set the crash model
# states <- matrix(4:1, nrow = 2)
# states
# states1 <- states[,rank(states[,2])]
# states1
# -----------------------------------------------------

# probability of each state $s1 and $s2 and $s3as well as dates that can be 
# placed against the crash state. 
inv <- c("HUF", "PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
prop <- list(c(inv))
for(i in inv){
table(list3[[i]]$posterior$state)
round(table(list3[[i]]$posterior$state)/
        length(list3[[i]]$posterior$state), digits = 4)[1]

# identify the rank of return
list3[[i]]$rank <- order(rank(list3[[i]]$pars[c(13, 15, 17)]))
# add proportions to list3
list3[[i]]$prop[1] <- round(table(list3[[i]]$posterior$state)/
                           length(list3[[i]]$posterior$state), digits = 4)[1]
list3[[i]]$prop[2] <- round(table(list3[[i]]$posterior$state)/
                          length(list3[[i]]$posterior$state), digits = 4)[2]
list3[[i]]$prop[3] <- round(table(list3[[i]]$posterior$state)/
                            length(list3[[i]]$posterior$state), digits = 4)[3]

# Get x, list of dates that refelect state1
list3[[i]]$dates[1] <- list3[[i]]$posterior$Date[which(list3[[i]]$
                                                       posterior$state == 1)]
list3[[i]]$dates[2] <- list3[[i]]$posterior$Date[which(list3[[i]]$
                                                       posterior$state == 2)]
list3[[i]]$dates[3] <- list3[[i]]$posterior$Date[which(list3[[i]]$
                                                       posterior$state == 3)]
prop[[i]][1] <- list3[[i]]$prop[list3[[i]]$rank[1]]
prop[[i]][2] <- list3[[i]]$prop[list3[[i]]$rank[2]]
prop[[i]][3] <- list3[[i]]$prop[list3[[i]]$rank[3]]
}
str(prop)
# This is a test.
list3[["CZK"]]$prop[3]
list3[["CZK"]]$prop2
list3[["CZK"]]$prop3
list3[["CZK"]]$rank
list3[[i]]$rank[1]

prop[[i]][2]
list3[[i]]$prop[3]
list3[[i]]$pars[13:18]
list3[[i]]$posterior$state
round(table(list3[["HUF"]]$posterior$state)/
                              length(list3[["HUF"]]$posterior$state), digits = 4)[1]

order(rank(list3[[i]]$pars[c(13, 15, 17)]))[2]
# Now extract the dates and proportions and relate to regime. 
order(rank(list3[["HUF"]]$pars[c(13, 15, 17)]))
list3[["HUF"]]$pars[o[1] + 12]
crashdate3R <- list(c(inv))
for(i in inv){
  if(list3[[i]]$prop2 > list3[[i]]$prop1){
    crashdate3R[[i]] <- list3[[i]]$dates1
  } else {
    crashdate3R[[i]] <- list3[[i]]$dates2
  }
}
str(crashdate3R)
# list of mode dates
crashvector2R <- as.Date(unlist(crashdate3R[inv]))
str(crashvector2R)
table(crashvector2R)
par(mfrow = c(1,1))
barplot(sort(table(crashvector2R), decreasing = TRUE)[1:10], las = 2, 
        main = "Frequency of Crash Periods")
#-----------------------------------
print(2Rcrashdate$HUF)
# Does not work.  How to print NBER recessions.
plot(list3[["HUF"]]$profit)
abline(h = 1)
x = as.Date(c(crashdate3R$HUF))
polygon(x, col = "red")
# Does not work.  Need coordinates for polygone. 
