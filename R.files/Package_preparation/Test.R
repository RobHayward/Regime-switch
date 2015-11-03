currency <- ls(pattern = "???EUR1")
for(i in currency){
  (i))
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
a <- forp("TRK", "EUR", 1)
eq1 <- lm(a$profit ~ da$D2)
summary(eq1)

