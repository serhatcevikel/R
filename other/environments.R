
gg <- function()
{
    a <- 1:3 
    #envv <- environment()
    #environment(ff) <- envv
    ff(a, 2, 5)
    return(a)
}


ff <- function(x, y, z)
{
    #environment() <- parent.frame()
    eval.parent(substitute(x[y] <- z))
    return(x)
}
