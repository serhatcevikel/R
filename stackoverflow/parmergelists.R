# How to merge two lists in parallel in R?

# http://stackoverflow.com/questions/41193164/how-to-merge-two-lists-in-parallel-in-r

A <- list(c(1,2,3), c(3,4,5), c(6,7,8))

B <- list(c("a", "b", "c"), c("d", "e", "f"), c("g", "h", "i"))

mergepar <- function(x = A, y = B) { # merge two lists in parallel
    ln <- max(length(x), length(y)) # max length
    newlist <- as.list(rep(NA, ln)) # empty list of max length

    for (i in 1:ln) { # for1, across length
        # two level subsetting (first with [ and then [[, so no subscript out of bound error) and lapply
        newlist[[i]] <- lapply(list(A, B), function(x) "[["("["(x, i), 1)) 
    }

    return(newlist)
}
