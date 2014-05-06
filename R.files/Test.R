# This is the file to test the code. Use this now to build the system to test
fx <- "HUF"
m <- 3
b <- "EUR"

M <- paste(m, "MD", sep = "")
M  
ra1 <- paste(fx, M, sep = "")
ra1  
ra2 <- paste(b, M, sep = "")
ra2  
das <- subset(da, select = c(fx, b, ra1, ra2))
head(das)  
tail(das)
class(das)
fw <- paste(ra1, "f", sep = "")

das$fw <- ((1 + das[,3]/100)^(m/12))/((1 + das[,4]/100)^(m/12))*(das[,1]/das[,2])
daz <- as.zoo(das, order.by = da$Date)
tail(daz)
head(daz)
daz$l1 <- lag(daz[,1], k = m)
daz$l2 <- lag(daz[,2], k = m)
head(daz)
# I change the columns 8 and 7 to 7 and 6 in the following line because VIX has been removed.  
daz$p <- (((daz[,1]/daz[,2])*(1 + daz[,3]/100)^(m/12))*(daz[,7]/daz[,6]))/
    (1 + daz[,4]/100)^(m/12)
  g <- list(data = daz, fx = fx, fund = b, period = M)
head(daz)
class(daz)
  g