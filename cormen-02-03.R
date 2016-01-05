# bubblesort algorithm, cormen page 40

bubblesort <- function(A) { # define function
lenA <- length(A) # length of input vector
    for (i in 1:(lenA-1)) { # define the index for the end of countdown
        for (j in lenA:(i+1)) { # define the index for the beginning of countdown
            if (A[j] < A[j-1]) { # define the condition for exchange
                tempA <- A[j] # temporary variable assignment for exchange
                A[j] <- A[j-1] # shift the bigger one to right
                A[j-1] <- tempA # shift the smaller one to left
            } # close if
        } # close for of j
    } # close for of i
A # return the sorted vector
} # close function
