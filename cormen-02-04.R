# horner's rule algorithm, for polynomials, cormen page 41

hornerser <- function(A,x) { # inputs are the vector of coefficients - A - and the x value
    degA <- length(A) - 1 # degree of A
    y <- 0 # variable for y value
    for (i in degA:0) { # start from the largest degree
        y <- A[i+1] + ( x * y ) # increment the y value
    } # close for of i
y # return last y value
} # close function
