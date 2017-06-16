FindOutliers <- function(data)
{
  lowerq = apply(data, 2, quantile)[2,]
  upperq = apply(data, 2, quantile)[4,]
  iqr = upperq - lowerq
  #identify extreme outliers
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  result <- list()
  for (i in 1:ncol(data))
  {
    result[[i]]<-which(data[,i] > extreme.threshold.upper[i] | data[,i] < extreme.threshold.lower[i])
  }
  return(result)
}
