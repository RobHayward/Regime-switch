# create dummy variables
source("R.files/Package_preparation/FW.R")
par(mfrow = (c(2,1)))
CrisisEvolution("HUF", "EUR", 1, 2, "2002-10-31", "2003-12-31")
CrisisEvolution("RON", "EUR", 1, 2, "2002-10-31", "2003-12-31")
CrisProb("PLN", "EUR", 1, 2, "2000-01-01", "2003-06-30")
# get the object not the string. 
currency <- ls(pattern = "???EUR1")
for(i in currency){
  (i)
}
plot(get(tempdata)$profit)
#-----------------------------------------
source("R.files/Package_preparation/FW.R")
#----D1: VIX---
for(i in 1:length(da$VIX)){
if(da$VIX[i] >= quantile(da$VIX, 0.90)){
  da$D1[i] = 1
}
else{
  da$D1[i] = 0
}
}
#--D2: US  rates--------------------------
for(i in 1:length(da$USD1MD)){
  if(da$USD1MD[i] >= quantile(da$USD1MD, 0.90)){
    da$D2[i] = 1
  }
  else{
    da$D2[i] = 0
  }
  }
#--D3: Ted-spread-------------------------
da$TED <- da$USD3MD - da$TBILL
for(i in 1:length(da$TED)){
  if(da$TED[i] >= quantile(da$TED, 0.90)){
    da$D3[i] = 1
      }
  else{
    da$D3[i] = 0
  }
}
#------Test regression of extreme.  Use D1, D2 or D3 or all together. 
# None seem to be significant
  a <- forp("TRK", "EUR", 1)
eq1 <- lm(a$profit ~ da$D2)
summary(eq1)
#----------------------------------
# The dates for capital mobility and crisis are in the file Data/RR_data.xls
# This looks at capital flows and banking crises
# Find list of key dates. From 