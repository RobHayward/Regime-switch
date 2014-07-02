# This is an update with the new data that runs to end 2013.  
# Vix has been removed. 
rm(list = ls())
require(zoo) # for lagging series
da <- read.csv("Data/CEEUIP.csv", header = TRUE, stringsAsFactors = FALSE)
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
# add USD
USD <- rep(1, length.out = nrow(da))
da$USD <- USD
head(da)
str(da)
require(xtable) # to create table
# This calculates forward rate and the carry profits, and then returns
# a list with the data.frame, the funding, investment and period as well as
# the carry profits series--------
forp <- function(fx, b, m){
  M <- paste(m, "MD", sep = "")
  ra1 <- paste(fx, M, sep = "")
  ra2 <- paste(b, M, sep = "")
  fw <- paste(ra1, "f", sep = "")
  # title can be uesd
  das <- subset(da, select = c(fx, b, ra1, ra2))
  das$fw <- ((1 + das[,3]/100)^(m/12))/((1 + das[,4]/100)^(m/12))*(das[,1]/das[,2])
  daz <- as.zoo(das, order.by = da$Date)
  daz$l1 <- lag(daz[,1], k = m)
  daz$l2 <- lag(daz[,2], k = m)
# I change the columns 8 and 7 to 7 and 6 in the following line because VIX has been removed. 
    daz$p <- (((daz[,1]/daz[,2])*(1 + daz[,3]/100)^(m/12))*(daz[,7]/daz[,6]))/
    (1 + daz[,4]/100)^(m/12)
  g <- list(data = daz, fx = fx, fund = b, period = M, profit = daz[,8])
  return(g)
}
#Test it
# a <- forp("HUF", "EUR", 1)
# head(a$data)
