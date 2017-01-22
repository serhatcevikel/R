# maximum subarray algorithm, cormen page 68-73
# hypens inside variable names are interpreted as minus. so hypens are replaced by underscores such as "left_sum"

maxser <- function(A) { # top level main function with only one argument: the vector
    high <- length(A) # assign the length as high index
    return(maxsubser(A,1,high)) # call the function with 3 arguments
} # close function

maxsubser <- function(A, low, high) { # divide and conquer function, recursive
    if (high == low) { # when the length is 1, if 1
        return(c(low, high, A[low])) # return the indices and the only element
    } else { # close if 1, open else 1
        mid <- floor((low + high)/2) # find the mid point, rounddown
        leftsubarray <- maxsubser(A, low, mid) # recursive for left subarray    
            left_low <- leftsubarray[1] # assign the index for low of left
            left_high <- leftsubarray[2] # assign the index for high of left
            left_sum <- leftsubarray[3] # assign the sum for left subarray

        rightsubarray <- maxsubser(A, mid + 1, high) # recursive for right sub
            right_low <- rightsubarray[1] # assign the index for low of right
            right_high <- rightsubarray[2] # assign the index for high of right 
            right_sum <- rightsubarray[3] # assign the sum for right subarray
    
        crosssubarray <- crossingsub(A, low, mid, high) # recursive for crossing sub
            cross_low <- crosssubarray[1] # assign the index for low of cross
            cross_high <- crosssubarray[2] # assign the index for high of cross 
            cross_sum <- crosssubarray[3] # assign the sum for cross subarray

        if (left_sum >= right_sum && left_sum >= cross_sum) { # if left sum is greater than the right and cross sums, if 2
            return(c(left_low, left_high, left_sum)) # return the values of left sub
        } else { # close if 2 and open else 2
            if (right_sum >= left_sum && right_sum >= cross_sum) { # if right sum is the greatest, if 3
                return(c(right_low, right_high, right_sum)) # return the values of right sub
            } else { # close if 3 and open else 3
                return(c(cross_low, cross_high, cross_sum)) # then the greatest is the cross, so its values are returned
            } # close else 3
        } # close else 2
    } # close else 1
} # close function
    

crossingsub <- function(A, low, mid, high) { # the max subarray which crosses the mid point, A is the vector; low, mid and high are indices on A
    left_sum <- -(Inf) # initial sum of the left part is minus inifinity
    sum <- 0 # initial sum is zero
    for (i in mid:low) { # to start from mid and sum to low, for 1
        sum <- sum + A[i] # update sum with a new index to the left
        if (sum > left_sum) { # condition to update the max of left sum from the mid to low, if 1
            left_sum <- sum # update left-sum if sum is larger than previous
            max_left <- i # index where the left sum is maximum
        } # close if 1
    } # close for 1 of i
    
    right_sum <- -(Inf)  # initial sum of the right part is minus inifinity
    sum <- 0 # initial sum is zero
    for (j in (mid + 1):high) { # to start from mid+1 and sum to high, for 2
        sum <- sum + A[j]
        if (sum > right_sum) { # condition to update the max of right sum from the mid to low, if 2
            right_sum <- sum # update right-sum if sum is larger than previous
            max_right <- j # index where the right sum is maximum
        } # close if 2
    } # close for 2 of j

    return(c(max_left, max_right, (left_sum + right_sum))) # return the indices of the max left and right values and the total of both left and right sums
    # multi-argument returns are not allowed, so return values are concatenated into a vector

} # close function

