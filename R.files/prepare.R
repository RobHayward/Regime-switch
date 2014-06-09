rm(list = ls())
da <- read.csv("Data/UIPdata2.csv", header = TRUE)
da$DATE <- as.Date(da$DATE, format = "%d/%m/%Y")
tail(da)
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
#====Automated---------------
require(depmixS4)
a <- forp("HUF", "EUR", 1)
tail(a$data)
#This is an attempt to create a list of data that can be tested.  The test
# is in Raw.R
i <- "UAH"
for(i in c("HUF", "PLN")){
  name <- paste(i, "EuR", sep = "")
  name
  a <- forp(i, "USD", 1)
  head(a)
  rets <- paste("P", name, sep = "")
  rets
  Data <- a$data$p
  head(Data)
  mod <- depmix(Data ~ 1, nstates = 3, data = da)
  set.seed(2)
  fm2 <- fit(mod, verbose = FALSE)
  pst <- posterior(fm2)
  dastate1 <- da$DATE[which(posterior(fm2)[,1] == 1)]
  dastate2 <- da$DATE[which(posterior(fm2)[,1] == 2)]
  dastate3 <- da$DATE[which(posterior(fm2)[,1] == 3)]
  s1 <- fm2@response[[1]][1]
  s2 <- fm2@response[[2]][1]
  s3 <- fm2@response[[3]][1]
  print(c(s1, s2, s3))
  title <- paste(rets, " Carry Trade", sep = "")
  pdf("Figures/UAHEUR.pdf", paper= "a4", title = title)
  par(mfrow = c(4,1))
  pst$Date <- index(Data)
  # This just adds a date series for the ppi file (the same as PPLNUSD)
  plot(Data, main = name, type = 'l')
  abline(h = 1)
  plot(pst[,2] ~ pst$Date, type = 'l', main = "Probability in State 1: Crash")
  plot(pst[,3] ~ pst$Date, type = 'l', main = "Probability in State 2: Caution")
  plot(pst[,4] ~ pst$Date, type = 'l', main = "Probability in State 3: Build")
  dev.off()
# This  last bit does not work.  Need to build the list with details.
}
name
head(datalist[3])
names(datalist)
head(datalist$PLN)
# This now works.
datalist[[2]][1][[9]]
# to choose the second item in list, first row and ninth element.
plot(datalist$HUF$p, main = "HUFEUR")

fm2@response[[2]][1][1]
# this will give the mean.  I want sd I don't know how to get that.  
predict(fm2@response[[3]][[1]])[1]

class(fm2@response)
names(fm2@response)
fm2@response

  