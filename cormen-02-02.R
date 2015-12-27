sersortmerge <- function(C) sersortmerge01(C,1,length(C)) # top level function with only one argument

sersortmerge01 <- function(B,p,r) { # divide-and-conquer algorithm for vector A from p to r elements
    if (p < r) { # condition when more than 1 elements
        q <- as.integer((p+r)/2) # devide: mid point of the p-r segment. works the same as "floor" function, rounds down to integer
        B[p:q] <- sersortmerge01(B,p,q) # conquer1: recursive for first part. when the assignment is not made back to the original vector through indexing, it doesn't work!
        B[(q+1):r] <- sersortmerge01(B,q+1,r) # conquer2: recursive for second part
        sermerge(B,p,q,r) # combine: merge in sort two parts
    }  else {
       B[p:r]
    }
}

sermerge <- function(A,p,q,r) { # merge part of the algorithm. p where subset 1 starts, q where it ends and subset 2 starts (included in subset1), r is where subset 2 ends
    L <- c(A[p:q],Inf) # copy original subset to a new vector Left part appended with infinity so that when the deck is finished we still have an element for comparison purposes
    R <- c(A[(q+1):r],Inf) # copy original subset to a new vector Right part
    i <- 1 # index for Left deck
    j <- 1 # index for Right deck
    for (k in p:r) { # index k for the original set
        if (L[i] <= R[j]) { # condition for taking a card out of Left deck
            A[k] <- L[i] # assing card in left deck to the original set
            i <- i + 1 # increment Left deck index
        } else {
            A[k] <- R[j] # if Right deck card is bigger, assign Right deck card to the original set
            j <- j + 1 # increment Right deck index
            }
        }
A[p:r]
}
