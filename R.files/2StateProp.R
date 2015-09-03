# List2 contains pars for the parameters of the model, the logkelihood 
# the posterior, a dataframe that contains the most likely state, $state, 
# This function comes from 
# http://stackoverflow.com/questions/2547402/
# standard-library-function-in-r-for-finding-the-mode
# -----------------------------------------------------
# This function comes from 
# http://stackoverflow.com/questions/2547402/
# standard-library-function-in-r-for-finding-the-mode
Mode <- function(x){
  ux <- unique(x)
  un[which.max(tabulate(match(x, ux)))]
}

# probability of each state $s1 and $s2 as well as dates that can be 
# placed against the most probable state. 
inv <- c("HUF", "PLN", "CZK", "RON", "RUB", "TRY", "BGN", 
         "NOK", "ISK", "UAH", "HRK")
for(i in inv){
table(list2[[i]]$posterior$state)
round(table(list2[[i]]$posterior$state)/
        length(list2[[i]]$posterior$state), digits = 4)[1]
# add proportions to list2
list2[[i]]$prop1 <- round(table(list2[[i]]$posterior$state)/
                           length(list2[[i]]$posterior$state), digits = 4)[1]
list2[[i]]$prop2 <- round(table(list2[[i]]$posterior$state)/
                          length(list2[[i]]$posterior$state), digits = 4)[2]
# Get x, list of dates that refelect state1
list2[[i]]$dates1 <- list2[[i]]$posterior$Date[which(list2[[i]]$posterior$state == 1)]
list2[[i]]$dates2 <- list2[[i]]$posterior$Date[which(list2[[i]]$posterior$state == 2)]
}
str(list2)
# I am not sure about crashdates.  This ranks on prop2 vs prop1.  Is that right? 
crashdate2R <- list(c(inv))
for(i in inv){
  if(list2[[i]]$prop2 > list2[[i]]$prop1){
    crashdate2R[[i]] <- list2[[i]]$dates1
  } else {
    crashdate2R[[i]] <- list2[[i]]$dates2
  }
}
str(crashdate2R)
# list of mode dates
crashvector2R <- as.Date(unlist(crashdate2R[inv]))
str(crashvector2R)
table(crashvector2R)
par(mfrow = c(1,1))
barplot(sort(table(crashvector2R), decreasing = TRUE)[1:10], las = 2, 
        main = "Frequency of Crash Periods")
#-----------------------------------
print(2Rcrashdate$HUF)
# Does not work.  How to print NBER recessions.
plot(list2[["HUF"]]$profit)
abline(h = 1)
x = as.Date(c(crashdate2R$HUF))
polygon(x, col = "red")
# Does not work.  Need coordinates for polygone. 
