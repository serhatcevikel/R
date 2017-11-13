# ibrahim's implementation

prime_1 <- function(x){
    for(i in 2:x)
    {
        np <- c()
        allnumb <- c(1:x)
        for(j in 2:(i-1))
        {
            if(i%%j == 0)
            {
                np <- c(np,i)
            }
        }
    }

    return(setdiff(allnumb,np))
}


# my modification on the code above
# works on x >= 3
prime_2 <- function(x){
    np <- 2
    for(i in 3:x)
    {
        if (!any(i %% np == 0))
        {
            np <- c(np, i)
        }
    }

    return(np)
}

## however this is not a so efficient approach
## the best way is to apply a sieve algorithm:
# delete the multiples of each number one by one up to some limit

# this is my sieve implementation
# could be enhanced by memory management
collect_primes <- function(y) # collect the primes upto y,sieve of Eratosthenes. Recycled from p037, p051, p203, p263
{
    if (y == 1) return(0) else # if 1, if x no prime then 0, else 1
    {

        if (y <= 3) return(2:y) else # if 2, x is 2, prime is 2, no iteration, else 2
        {
            sequencep <- seq(from = 3, to = y, by = 2) # a sequence of numbers from 3 to y
            primes2 <- 2 # vector of primes, start with 2
            
            while (sequencep[1] <= ceiling(sqrt(y))) # until the first item reaches the square root of y - after that we do not have to look, since primes can show up to that point
            {
                primes2 <- c(primes2, sequencep[1]) # update the vector of primes with the first item of sequence, always a prime
                sequencep <- sequencep[sequencep %% sequencep[1] != 0] # update sequence 2 with items that are not diivisible by the first item 
            } # close while

            return(c(primes2, sequencep)) # first item of the remaining vector is the xth prime

        } # close else 2

    } # close else 1

} # close function


# this one tracks the primes and multiples not by deleting a vector but toggling boolean values
# the multiples are extracted by a sequence, not modulo
collect_primes2 <- function(limitt) # this works with a boolean vector
{
    bool_vec <- rep(T, limitt)
    bool_vec[1] <- F
    number_vec <- c(2, 1 + 2 * (1:ceiling(sqrt(limitt) / 2)))

    for (num in number_vec)
    {
 
        if (bool_vec[num])
        {
            indices <- num * (2:floor(limitt / num))
            bool_vec[indices] <- F
        }
 
    }

    primes <- which(bool_vec)

    return(primes)

}

    
