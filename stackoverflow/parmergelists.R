# How to merge two lists in parallel in R?

# http://stackoverflow.com/questions/41193164/how-to-merge-two-lists-in-parallel-in-r

A <- list(c(1,2,3), c(3,4,5), c(6,7,8))

B <- list(c("a", "b", "c"), c("d", "e", "f"), c("g", "h", "i"))

mergepar <- function(x = A, y = B) {
    ln <- max(length(A), length(B))
    newlist <- as.list(rep(NA, ln))
    for (i in 1:ln) {
        newlist[[i]] <- lapply(list(A, B), function(x) "[["("["(x, i), 1))
    }
    return(newlist)
}
