# R: replicate a row and update the date entry by one per row

# Answer to http://stackoverflow.com/questions/41360278/r-replicate-a-row-and-update-the-date-entry-by-one-per-row

# The input and its intended output show that I want to replicate the row of the input and update the date entry. How can I do this?

# Input

# > aa<- data.frame(a=c(1,11,111),b=c(2,22,222),length=c(3,5,1),date=c(as.Date("28.12.2016",format="%d.%m.%Y"), as.Date("30.12.2016",format="%d.%m.%Y"), as.Date("01.01.2017",format="%d.%m.%Y")))
# > aa
#     a   b length       date
# 1   1   2      3 2016-12-28
# 2  11  22      5 2016-12-30
# 3 111 222      1 2017-01-01

# Intended Output

#   a   b length       date
# 1 1   2      3 2016-12-28
# 2 1   2      3 2016-12-29
# 3 1   2      3 2016-12-30
# 4 11  22     5 2016-12-30
# 5 11  22     5 2016-12-31
# 6 11  22     5 2017-01-01
# 7 11  22     5 2017-01-02
# 8 11  22     5 2017-01-03
# 9 111 222    1 2017-01-01


aa<- data.frame(a=c(1,11,111),b=c(2,22,222),length=c(3,5,1),date=c(as.Date("28.12.2016",format="%d.%m.%Y"), as.Date("30.12.2016",format="%d.%m.%Y"), as.Date("01.01.2017",format="%d.%m.%Y")))        

replicaterow <- function(df1 = aa) {
    lastrow <- df1[nrow(df1),]
    lastrow[4] <- lastrow[4] + 1
    df1 <- rbind(df1, lastrow)
    return(df1)
}


replicaterow1 <- function(df1 = aa) {
    newdf <- df1[0,]
    rowss <- nrow(df1)
    rowcount <- 1
    for (i in 1:rowss) {
        rowi <- df1[i,]
        reps <- as.integer(rowi[3])
        newrow <- rowi
        newdf[rowcount,] <- rowi
        rowcount <- rowcount + 1
        if (reps > 1) {
            for(j in 1:(reps-1)) {
                newrow[4] <- newrow[4] + 1
                newdf[rowcount,] <- newrow
                rowcount <- rowcount + 1
            }
        }
    }
    return(newdf)
}
