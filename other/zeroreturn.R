# get share of zero return counts in each month

countifzero <- function() {
    data1 <- read.csv("Countif.csv")
    averages <- aggregate(data1[,-(1:2)], by = list(data1[,1], data1[,2]), FUN = function(x) length(x[x == 0]) / length(x))
    write.csv(averages, file = "averages.csv")
    return(averages)
}
