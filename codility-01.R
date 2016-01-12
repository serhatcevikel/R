# lesson 1 in https://codility.com/programmers/
# BinaryGap
# Find longest sequence of zeros in binary representation of an integer
#
# https://codility.com/programmers/task/binary_gap/
# solution written by Serhat Cevikel.
#   
#
# built in functions intToBits and strsplit are not used in order to exercise the lower level algorithms
#
# manual:
# use form_bin() to convert a vector of numbers into alternating runs of 1s and 0s with the times of repetition taken from the vector.
#       odd indexed items of the input vector are the times 1's are repeated and vvi.
#       in order to bypass the integer overflow, binary numbers are represented as a string
# use bin2dec() to convert the binary number given by the form_bin()
# use dec2bin() to convert the decimal number to binary as a string - can be utilized to validate bin2dec(). binary gap uses this function
# use binarygap() to find the length of the maximum run of consecutive 0's in the binary representation of the decimal input
#      note that the output of the binarygap() must be equal to the largest even indexed number in the input vector to form_bin() 

binarygap <- function(deci) { # function for funding the length of the longest sequence of 0's in the binary representation of a number
    bin_string <- dec2bin(deci) # convert the decimal to binary, as a string
    bin_length <- nchar(bin_string) # get the length of bin_integer
    digits_inv <- numeric(bin_length) # vector for digits 
    digits_inv <- 1- as.integer(substring(bin_string, seq(bin_length), seq(bin_length))) # split the string into characters, turn into integer, inverts the digit and place in vector. strsplit does this as well
    cum_sum_digits <- function(upto) cum_sum(digits_inv, upto) # simplify the cum_sum_digits function to one element
    digits_inv_sums <- sapply(1:bin_length, cum_sum_digits, simplify = TRUE) # return the cumulative sum of digits for all indices
    #digits_sums2 <- mapply(cum_sum, digits, 1:bin_length, simplify = TRUE)
    max_0 <- max(digits_inv_sums) # maximum value of digist_sums, maximum number of consecutive 0's
    return(max_0) # return the max
} # close function

cum_sum <- function(digits_inv, upto) { # cumulative sum of the elements of a vector upto an index
    sum <- 0 # initial sum of zero
    for (i in 1:upto) { # for 1 segment upto a point
        sum <- (sum + digits_inv[i])*digits_inv[i] # find the max sum of consecutive 1's, 1 increments the sum while 0 resets
    } # close for 1
return(sum) # return the sum
} # close function

dec2bin <- function(decim) convert_to_binary("",decim) # function decimal to binary with only one argument

convert_to_binary <- function(bin, decim) { # function to convert a decimal number to binary as character, starting bin should be 0
    if (decim > 1) { # if more than one digit, then recursion
        digit <- decim %% 2 # the new digit is the modulo from division by 2 - divide 
        decim <- floor(decim / 2) # new decim is the floor of divison by 2 - divide
        bin <- paste(digit, bin, sep = "") # append the new digit to the left as character - conquer
        convert_to_binary(bin, decim) # recurse the division
    } else { # close if 1, else 1 for atomic condition
        bin <- paste(decim, bin, sep = "")  # append remaining to the left of binary representations
        return(bin)
    } #close else 1
} # close function

bin2dec <- function(bin) { # convert binary to decimal
    bin <- as.character(bin) # make the binary number a string of 1's and 0's
    places <- seq(nchar(bin)) # a vector of a sequence from 1 to the number of digits
    digits <- as.integer(substring(bin, places, places)) # form a vector of separate integers of the binary
    deci <- sum(digits[rev(places)] * (2^(places-1))) # convert each binary digit to decimal and sum them up
    return(deci) # return decimal value
} # close function

form_bin <- function(sequ) { # convert a sequence of 1's and 0's to a binary number. the odd indices are consecutive 1's while even indices are consecutive 0's
    runs <- length(sequ) # find the number of runs of 1's and 0's, the length of sequ
    a1s0s <- numeric(runs) # form an empty vector of length runs
    a1s0s[1:runs] <- (1:runs)%%2 # fill the vector with altering 1's and 0's
    bin <- paste(rep(a1s0s[1:runs], sequ[1:runs]), sep = "", collapse = "") # repeat the 1's and 0's with corresponding times in sequ into a binary
    return(bin) # return the binary value
} # close function

