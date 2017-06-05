#myStats
# This will create descriptive statistics for all your needs
myStats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x) 
  t <- (m-1)/(s/n^0.5)
  med <- median(x)
  skew <- sum((x-m)^3/s^3)/n
  ses <- ((6*n*(n-1))/((n-1)*(n+1)*(n+3)))^0.5
  kurt <- sum((x-m)^4/s^4)/n - 3
  sek <- ((n^2-1)/((n-3)*(n + 5)))^0.5
  max <- max(x)
  min <- min(x)
  return(c(n=n, mean = 100*(m^12 - 1), Sharpe = t, medium = 100*(med^12 - 1), 
             stdev=s, skew=skew, ses = ses,  kurtosis=kurt, sek = sek, 
           max = 100*(max-1), min = 100*(min-1)))
}
