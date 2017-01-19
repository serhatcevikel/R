# http://stackoverflow.com/questions/41491178/r-comparing-values-in-vector-to-column-in-data-frame

responseTimes <- c(150, 50, 250, 200, 100, 150, 250)
bins1 <- seq(0, 250, by = 50)


sahil1 <- function(input = responseTimes, binsx = bins1) {
    tablem <- table(cut(input, binsx)) # count of input across bins
    tablem <- cumsum(tablem) # cumulative sums
    return(as.data.frame(tablem)) # table to data frame
}
