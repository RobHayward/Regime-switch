source("R.files/Package_preparation/LoadData.R")
# fx is the investent currency, base is base currency and m is the timeframe
# currency can come from inv list, base from base list, m = c(1, 3)
forp <- function(fx, b, m){
  M <- paste(m, "MD", sep = "")
  ra1 <- paste(fx, M, sep = "")
  ra2 <- paste(b, M, sep = "")
  fw <- paste(ra1, "f", sep = "")
  title <- paste(fx, b, m, sep = "")
  # title can be uesd
  das <- subset(da, select = c(fx, b, ra1, ra2))
  das$fw <- ((1 + das[,3]/100)^(m/12))/((1 + das[,4]/100)^(m/12))*(das[,1]/das[,2])
  daz <- as.zoo(das, order.by = da$Date)
  daz$l1 <- lag(daz[,1], k = m)
  daz$l2 <- lag(daz[,2], k = m)
  daz$p <- (((daz[,1]/daz[,2])*(1 + daz[,3]/100)^(m/12))*(daz[,7]/daz[,6]))/
    (1 + daz[,4]/100)^(m/12)
  g <- list(data = daz, fx = fx, fund = b, period = M, profit = daz[,8], 
            title = title)
  return(g)
}
