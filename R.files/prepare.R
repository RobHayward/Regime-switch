rm(list = ls())
da <- read.csv("UIPdata2.csv", header = TRUE)
da$DATE <- as.Date(da$DATE, format = "%d/%m/%Y")
head(da)
str(da)
# call(prepare.R) # If needed------------------------------
require(zoo) # for lagging series
require(xtable) # to create table
# This calculates forward rate and the profits and creates data frame--------
forp <- function(fx, b, m){
  M <- paste(m, "M", sep = "")
  ra1 <- paste(fx, M, sep = "")
  ra2 <- paste(b, M, sep = "")
  # this line to to name the column in the dataframe but does not work yet
  fw <- paste(ra1, "f", sep = "")
  title <- paste(fx, "-", b, "fw", sep = "")
  das <- subset(da, select = c(fx, b, ra1, ra2, "VIX"))
  das$fw <- ((1 + das[,3]/100)^(m/12))/((1 + das[,4]/100)^(m/12))*(das[,1]/das[,2])
  daz <- as.zoo(das, order.by = da$DATE)
  daz$l1 <- lag(daz[,1], k = m)
  daz$l2 <- lag(daz[,2], k = m)
  daz$p <- (((daz[,1]/daz[,2])*(1 + daz[,3]/100)^(m/12))*(daz[,8]/daz[,7]))/
    (1 + daz[,4]/100)^(m/12)
  g <- list(data = daz, fx = fx, fund = b, period = M)
  return(g)
}
a <- forp("HUF", "EUR", 1)
head(a$data)
#This is an attempt to create a list of data that can be tested.  The test
# is in Raw.R
datalist <- list()
for(i in c("HUF", "PLN")){
  name <- paste(i, "EuR", sep = "")
  a <- forp(i, "EUR", 1)
datalist[i] <- list(name = a$data) 
}
names(datalist)
head(datalist$PLN)
# This now works.
datalist[[2]][1][[9]]
# to choose the second item in list, first row and ninth element.
plot(datalist$HUF$p, main = "HUFEUR")

