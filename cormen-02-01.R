sersortinsert <- function (x) { #x is a vector of numbers to be sorted
    for ( j in 2:length(x) ) { # loop for comparing element j
        key <- x[j] # comparing element 
        i <- j - 1 # index for element to be compared
        while ( (i > 0)  && (x[i] > key) ) { # continue condition to compare
            x[i + 1] <- x[i]; # shift the larger value right
            x[i] <- key; # shift the comparing element -key- left  
            i <- i - 1; # shift compare index left
        }
    }
return(x)
}
 
