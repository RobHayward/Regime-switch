# DesTab.R
# This will create a table of descriptive statistics for the UIP paper
# Use the myStats function on the tempfile list of potential carry-trade returns
require(xtable)
inv <- c("HUF", "PLN", "CZK", "RON",  "RUB", "BGN", 
         "NOK", "ISK", "UAH", "HRK", "TRY")
DesTab <- matrix(NA, nrow = 11, ncol = length(inv))
rownames(DesTab) <-  c("Number", "Mean","Sharpe", "Medium", "StDev",
             "Skew", "SES", "Kurt", "SEK", "Max", "Min")
colnames(DesTab) <- inv
for(i in inv){
  temp <- myStats(unlist(list2[['EUR']][[i]][6]), na.omit = TRUE)
  DesTab[,i] <- round(temp,2)
}
DesTab
tab <- xtable(DesTab)
tab
