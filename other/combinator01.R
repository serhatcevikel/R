# algorithm for listing all n sized combinations of the items in a vector
# aimed to emulate combn() built in function combined with t()
# scans exhaustively from left to right
# comments are welcomed to clean the environment after the function returns (the global variables)

combinator <- function (vec, size) { # function to list all the "size" sized combinations of the elements in vector vec
    len_vec <- length(vec) # length of the vector
    all_combs <<- factorial(len_vec)/(factorial(size)*factorial(len_vec - size)) # number of all combinations
    combs_mat <<- matrix (nrow = all_combs , ncol = size) # create an empty matrix for all combinations
    comb_count <<- 1 # combination count
    level_count <- 0 # level count
    index_vector <- numeric(size) # holder of the indices
    combs_mat2 <- combinator_recursive(vec, size, level_count, index_vector) # recursive function
    return(combs_mat2) # return the matrix
} # close function

combinator_recursive <- function(vec_level, size, level_count, index_vector) { # recursive part of the algorithm
    if(level_count < (size - 1)) { # if 1, until we arrive at the last term of the combination
        level_count <- level_count + 1 #increment the level
        end_of_vector <- max(1, (length(vec_level) - size + level_count)) # last iteration to reach
        for (i in 1:end_of_vector) { # for 1, loop for updating the row vector for the level and moving to the next level
            index_vector[level_count] <- vec_level[i] # update the row vector
            vec_level_segment <- vec_level[-(1:i)] # get the the remaining vector
            combinator_recursive(vec_level_segment, size, level_count, index_vector) # recursion
        } # close for 1
        level_count <- level_count - 1
    } else { # close if 1, else 1, last level
        level_count <- level_count + 1 # increment the level
        end_of_vector <- max(1, length(vec_level))
        for (i in 1:end_of_vector) { # for 2, update the matrix
            index_vector[level_count] <- vec_level[i] # last iteration of segment
            combs_mat[comb_count, ] <<- index_vector # update the original matrix
            if (comb_count == all_combs) next else { # if 2 for break, else 2 
                comb_count <<- comb_count + 1 # increment the comb count
            } # close else 2
          } # close for 2
    } # close else 1
    return(combs_mat) # return the matrix  
} # close function
        
            
            
            
