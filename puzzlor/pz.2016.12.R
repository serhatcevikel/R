# http://www.puzzlor.com/2017-02_PortInAStorm.html
# Feb 2017 - Any Port in A Storm
#
# In the Market for a Spaceship?
# A popular game in the Apple and Google app stores is a space adventure game where, among other things, you need to decide which spaceship to buy.  There are dozens of different spaceships to choose from all with varying capabilities and costs.
# Ten of these ships are shown in the accompanying table.  Although there are additional ship capabilities in the game, only consider the ones shown in the table.
# Question:  Which is the most undervalued ship?

datax <- read.csv("./spaceship.csv", stringsAsFactors = F)

row.names(datax) <- datax[,1]
datax <- as.matrix(datax[,-1])

undervalued.ship <- function(data.1 = datax)
{
    n.ships <- nrow(data.1)
    combs <- t(combn(1:n.ships, 3))

    solutions <- t(apply(combs, 1, solve.price, data.1))
    solutions <- solutions[!is.na(solutions[,1]),]

    calculated.prices <- solutions %*% t(data.1[,1:3])
    median.prices <- apply(calculated.prices, 2, median)
    premium <- data.1[,4] / median.prices - 1
    result.1 <- which.min(premium)

    premium.mat <- data.1[,4] / calculated.prices - 1
    results.vec <- apply(premium.mat, 1, which.min)
    results.tab <- table(results.vec)
    result.2 <- which.max(results.tab)

    calculated.prices2 <- lm(datax[,4] ~ datax[,1:3] - 1)[[2]]
    result.3 <- which.min(data.1[,4] / calculated.prices2 - 1)

    return(row.names(datax)[c(result.1, result.2, result.3)])

}


dominance <- function(data.1 = datax)
{
    data.1[,4] <- -data.1[,4]
    rw <- nrow(data.1)
    mat <- data.1
    dom.mat <- outer(1:rw, 1:rw, FUN = compare.dom.v, list(mat))
    return(dom.mat)
}



compare.dom <- function(row1, row2, mat)
{
    vec1 <- mat[row1,]
    vec2 <- mat[row2,]

    dom <- all(vec1 >= vec2)
    return(dom)
}

compare.dom.v <- Vectorize(compare.dom)



solve.price <- function(rows, data.1)
{
    rowss <<- rows
    mat <- data.1[rows,] # subset the matrix for the selected ships
    unit.prices <- rep(NA, 3)
    try(unit.prices <- solve(mat[,1:3], mat[,4]))
    return(unit.prices)
}
