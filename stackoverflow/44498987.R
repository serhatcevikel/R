table1 <- read.table("data/44498987-1", header = T)
table2 <- read.table("data/44498987-2", header = T)

times1 <- strptime(table1[,2], "%d-%b-%Y %I:%M:%S %p")
times2 <- strptime(table2[,2], "%d-%b-%Y %I:%M:%S %p")

table1a <- data.frame(table1[,1], times1)
names(table1a) <- c("id", "date_time")
table2a <- data.frame(table2[,1], times2, table2[,3])
names(table2a) <- c("id", "date_time", "cum_freq")

table2list <- split(table2a, table2a[,1])


getcumul <- function(x)
{
    x <- table1a[x,]
    df1 <- table2list[[x$id]]
    cutt <- findInterval(x$date_time, df1[,2])
    if (cutt == 0) return(0) else {
    cumul <- df1[cutt,3]
    return(cumul) }
}


cumuls <- function() sapply(1:nrow(table1a), getcumul)

