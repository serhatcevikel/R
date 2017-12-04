# calculate the real root(s) of a quadratic polynomial
# coefficients of which are given as aa, bb and cc
quadratic_roots <- function(aa = 1, bb = 2, cc = 1)
{
    # calculate the discriminant
    disc <- bb^2 - 4 * aa * cc

    if (disc == 0) # if1, single root
    {
        return(-bb / (2 * aa))
    }
    else
    {
        if (disc > 0) # if2, two roots
        {
            root1 <- (-bb - sqrt(disc)) / (2 * aa)
            root2 <- (-bb + sqrt(disc)) / (2 * aa)
            return(c(root1, root2))

        }
        else # no real roots, return NA
        {
            return(NA)
        }
    }
}


