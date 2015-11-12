#rm(list = ls())
require(zoo) # for lagging series
da <- read.csv("Data/CEEUIP.csv", header = TRUE, stringsAsFactors = FALSE)
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
# add USD
USD <- rep(1, length.out = nrow(da))
da$USD <- USD
rm(USD)
#--------------------------------