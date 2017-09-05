## package for rolling sum, mean, min, max operations

if (!require("RcppRoll", character.only = T, quietly = T)) { # if1 the package is not installed
    install.packages("RcppRoll") # install the package
} # close if1
require("RcppRoll", character.only = T) # and load it

library(parallel)

## note:
## run the function as a <- omer01(<parameters>)
## to get the input matrix a[[1]], to get the output matrix a[[2]]



# upperlimit: max window size
# shufflecount: how many runs for random shuffle
# returns is the input from 
omer01 <- function(upperlimit = 120, shufflecount = 1000, returns = NULL) {

    if (is.null(returns)) # if no return vector is supplied
    {
        returns <- runif(568) # create random numbers for excel vector 
    }
    else
    {
        if (is.data.frame(returns)) returns <- returns[[1]] # if data frame, convert to numeric vector
    }

    ## prepare input and empty output matrices
    # note that matrix is transposed automatically, good for rolling sum sub-routine below
    rep1 <- replicate(shufflecount, sample(returns)) # shuffle vector with sample and replicate with shufflecount, input matrix
    rep.var <- matrix(rep(0, upperlimit * shufflecount), ncol = upperlimit) # columns for each windows size and rows for each shuffle


    for (i in 1:upperlimit) # across window sizes
    {
        rep.var[,i] <- getvar(i, rep1) # get the variances of rolling sums of window size i
    }

    result <- list(t(rep1), rep.var) # combine the (transposed) input and output matrices as a list
    names(result) <- c("input_mat", "output_mat")

    return(result)

}


omer02 <- function(upperlimit = 120, shufflecount = 1000, returns = NULL) {

    if (is.null(returns)) # if no return vector is supplied
    {
        returns <- runif(568) # create random numbers for excel vector 
    }
    else
    {
        if (is.data.frame(returns)) returns <- returns[[1]] # if data frame, convert to numeric vector
    }

    ## prepare input and empty output matrices
    # note that matrix is transposed automatically, good for rolling sum sub-routine below
    rep1 <- replicate(shufflecount, sample(returns)) # shuffle vector with sample and replicate with shufflecount, input matrix
    #rep.var <- matrix(rep(0, upperlimit * shufflecount), ncol = upperlimit) # columns for each windows size and rows for each shuffle

    ## multicore parallelized version of the loop. > x 4 performance enhancement
    rep.var <- parallel::mcmapply(getvar, 1:upperlimit, list(rep1), mc.cores = parallel::detectCores())

    result <- list(t(rep1), rep.var) # combine the (transposed) input and output matrices as a list
    names(result) <- c("input_mat", "output_mat")

    return(result)

}

getvar <- function(i, repp = rep1) # input matrix and window size, get rolling sums and variances
{
    summed_mat <- RcppRoll::roll_sum(repp, i) # batch moving sums
    vars <- apply(summed_mat, 2, var) # apply var to columns (note that each column is a shuffle since replicate transposes automatically

    return(vars)
}


