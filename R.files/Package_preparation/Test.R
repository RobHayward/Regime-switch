# create dummy variables
source("R.files/Package_preparation/FW.R")
D1 <- rep(0, length(da$VIX))
D1 <- da$D1[which(da$VIX >= quantile(da$VIX, 0.80)),]          


# get the object not the string. 
currency <- ls(pattern = "???EUR1")
for(i in currency){
  (i)
}


plot(get(tempdata)$profit)

