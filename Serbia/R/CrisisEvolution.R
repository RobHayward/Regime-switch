source("R.files/Package_preparation/LoadData.R")
source("R.files/Package_preparation/FW.R")
source("R.files/Package_preparation/CrisisProb.R")
CrisisEvolution <- function(inv, fund, period = 1, reg.number = 2, 
                            start.date = "2000-01-01", end.date = "2013-12-31"){
# Date sequence go to new month and back one for end of month
# http://stackoverflow.com/questions/8333838/how-do-you-generate-a-sequence-of
#-the-last-day-of-the-month-over-two-years-in-r
  dates <- seq(from = as.Date(start.date) +1, to = as.Date(end.date) +1, by = 
                 "1 month") -1 
    crashlist <- rep(NA, length.out = length(dates))
    for(i in 1:length(dates)){
    crashlist[i] <- CrisProb(inv, fund, period, reg.number = 2, 
                             end.date = dates[i])
    }
  plot(dates, crashlist, ylab = "Probability of instability", main = 
         "Evolution of Instability over time", type = 'l')
  return(crashlist)
  }
