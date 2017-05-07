# restricted part size partitions
# with dynamic programming (recursion and memoization)

## https://en.wikipedia.org/wiki/Partition_%28number_theory%29#Partition_function
## restricted part size or number of parts

## By taking conjugates, the number pk(n) of partitions of n into exactly k parts is equal to the number of partitions of n in which the largest part has size k.[23] The function pk(n) satisfies the recurrence

## pk(n) = pk(n − k) + pk−1(n − 1)
## with initial values p0(0) = 1 and pk(n) = 0 if n ≤ 0 or k ≤ 0. 


partitions <- function(n, k) { # number of ways partitioning total n into k groups - non-ordered
    part.mat <<- matrix(nrow = n, ncol = n) # empty matrix for memoization. k for rows, n for columns
    result <- partitions.rec(n,k) # recursive function
    return(result)
}

partitions.rec <- function(n,k) { # recursive function to calculate partitions
    if (k >=0 & k == n) { # if1 n and k are 0 or more and the same
        return(1)
    } else { # close if1, else1
        if (k <= 0 | n <= 0) { # if2 at least one is less than or equal to 0
            return(0)
        } else { # close if2, else2
            current.val <- part.mat[k,n] # subset the matrix
            if (is.na(current.val)) { # if3, currently NA
                part.val <- partitions.rec(n-k, k) + partitions.rec(n-1, k-1) # recurse the function
                part.mat[k,n] <<- part.val # update the matrix
                return(part.val)
            } else return(current.val) # close if3, else 3, return the current value
        } # close else2
    } # close else1
} # close function
