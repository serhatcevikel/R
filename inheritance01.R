# the environment of variable, global assignment, etc

f <- function(x) {
    a <<- 2 # global assignment
    return(g(x))
}

g <- function(y) {
   a <- (a * y) # the global copy is used
    return(a)
}
