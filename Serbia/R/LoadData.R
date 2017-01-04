#rm(list = ls())
require(zoo) # for lagging series
da <- read.csv("Serbia/Data/BalkLend.csv", header = TRUE, stringsAsFactors = FALSE)
da$Date <- as.yearmon(da$Date)
head(da)
tail(da)
#--------------------------------