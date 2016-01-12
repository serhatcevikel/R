# algorithm to solve a sudoku puzzule of 9x9
# if n cells has at most n different digits at most in a row, column or 3x3, delete those digits from other cells. If there is a digit which exists only in one cell among the n cells, leave it alone
# if a digit exists only in a cell in a row, column or 3x3, leave it alone there
# if a digit exists only in the cells of a single row (column) inside 3x3, delete those digits in the same row (column) outside the 3x3
# this code is dedicated to my wife
# first install "xlsx" package by install.packages("xlsx")

library("xlsx") #include library xlsx to read data from xls file

assign("length_array", NULL) # define the log of length of array
assign("length_mat", NULL) # define the lof og length of array
assign("sudoku01", NULL) # define sudoku01 as global variable
assign("sudoku_ar",NULL) # define sudoku_ar as global variable
assign("sudoqu_sq",NULL) # define sudoku_sq as global variable
assign("combinations",NULL)


length_append <- function(a,b) { # function to append the non NA length of sudoku_ar
    len <- length(b[is.na(b) == FALSE]) # length of the array
    a <- c(a, len) # append the log of the length of array
    return(a)
} # close function


sudoku2 <- function() { # just to show the initial sudoku matrix
    sudoku02 <- as.matrix(read.xlsx("sudoku.xlsx", sheetName = "Sheet1", header = FALSE)) # read sudoku data from file into a matrix names sudoku01
return(sudoku02) # return the initial sudoku matrix
} # close function


sudoku <- function() {
    sudoku01 <<- as.matrix(read.xlsx("sudoku.xlsx", sheetName = "Sheet1", header = FALSE)) # read sudoku data from file into a matrix names sudoku01
    assign("sudoku01", sudoku01) # make it a global variable
    length_array <<- NULL # number of non NA's in sudoku_ar, NULL at initial
    length_mat <<- NULL # number of non NA's in sudoku matrix, NULL at initial
    colnames(sudoku01) <- NULL # replace the column names with NULL
    sudoku_sq <- sudoku_squares() # matrix for numbering 3x3 squaresi
    assign("sudoku_sq", sudoku_sq) # assign sudoku squares as a global variable
    sudoku_ar <<- form_array() # form 3d array of sudoku
    assign("sudoku_ar", sudoku_ar) # assign sudoku ar as a global variable
    length_array <<- length_append(length_array, sudoku_ar)
    length_mat <<- length_append(length_mat, sudoku01) # length_mat updated with initial matrix
    sudoku_ar <<- clear_single_first(sudoku01, sudoku_ar) # clear the cells for singled-out values in the array
    length_array <<- length_append(length_array, sudoku_ar)
    length_mat <<- length_append(length_mat, sudoku01) # length_mat updated with initial matrix
    combined_clear(sudoku01, sudoku_sq, sudoku_ar)
    length_array <<- length_append(length_array, sudoku_ar)
    length_mat <<- length_append(length_mat, sudoku01) # length_mat updated with initial matrix
    output <<- rbind(length_array, length_mat) # combine the length_array and length_mat into a matrix
    assign("sudoku01",sudoku01) # assign sudoku01 as a global variable
    return(sudoku01)
} #close function


form_array <- function() { # form a 3d array for 1:9
    data <- numeric(0) # create empty object
    for (i in 1:9) data <- c(data, rep(i, times=81)) # form a vector of 1s...9s
    sudoku_arc <- array (data, dim = c(9,9,9), dimnames = NULL) # transform the vector to an array
    return(sudoku_arc) # return the array
} # close function


combined_clear <- function(sudoku01c, sudoku_sqc, sudoku_arc) { # combine clear_single_first and clear_sudoku01 until no more gains
    count_sudoku_arc1 <- Inf # define the first count of non NA cells
    count_sudoku_arc2 <- length(sudoku_arc[is.na(sudoku_arc)==FALSE]) #number of non NA cells in sudoku_arc
    while (count_sudoku_arc2 < count_sudoku_arc1) { # until the number of non NA cells decrease repeat the functions
        count_sudoku_arc1 <- count_sudoku_arc2 # make the previous count 2 the new count 1
        sudoku_arc <- clear_single(sudoku01c, sudoku_sqc, sudoku_arc) # clear sudoku_arc off digits
        sudoku_arc <- leave_the_orphan(sudoku01c, sudoku_sqc, sudoku_arc) # leave any digits that shows only on one cell in row, column or 3x3 is left alone
        sudoku_arc <- clear_outside_3x3(sudoku01c, sudoku_sqc, sudoku_arc) # leave any digits that shows only on one cell in row, column or 3x3 is left alone
        sudoku_arc <- multiple_clear_full_row(sudoku01c, sudoku_sqc, sudoku_arc) # clean rows for multiple common items
        sudoku_arc <- multiple_clear_full_col(sudoku01c, sudoku_sqc, sudoku_arc) # clean columns for multiple common items
        sudoku01c <- clear_sudoku01(sudoku01c, sudoku_arc) # update sudoku01 
        sudoku_arc <- clear_single(sudoku01c, sudoku_sqc, sudoku_arc) # clear sudoku_arc off digits
        count_sudoku_arc2 <- length(sudoku_arc[is.na(sudoku_arc)==FALSE]) #number of non NA cells in sudoku_arc
    } # close while    
    sudoku01 <<- sudoku01c # update the global object sudoku01
    sudoku_ar <<- sudoku_arc # update the global object sudoku_ar
    sudoku_sq <<- sudoku_sqc # update (assign) the global object sudoku_sq
    assign("sudoku_ar", sudoku_ar) # assign sudoku_ar to global variable
    assign("sudoku01", sudoku01) # assign sudoku_ar to global variable
    assign("sudoku_sq", sudoku_sq) # assign sudoku_ar to global variable
    return(count_sudoku_arc2) # return the last count
} # close function           


clear_single_first <- function(sudoku01c, sudoku_arc) { # for each singled-out (non empty) cell in sudoku01, clear vector on the same row and column in sudoku_ar
    for (i in 1:9) { # for 1 rows in sudoku01
        for (j in 1:9) { # for 2 columns in sudoku01
            if (is.na(sudoku01c[i,j]) == FALSE) { # if 1 the cell in sudoku01 is non empty
                sudoku_arc[i,j,-(sudoku01c[i,j])] <- NA # empty the vector except for the singled-out value
            } # close if 1
        } # close for 2
    } # close for 3
    sudoku_ar <<- sudoku_arc # save the local copy to global variable
    assign("sudoku_ar", sudoku_ar) # assign sudoku_ar to global variable
    return(sudoku_ar) # return the cleaned sudoku_ar
} # close function


clear_single <- function(sudoku01c, sudoku_sqc, sudoku_arc) { ## for each singled-out (non empty) cell in sudoku01, clear the digit from the row, column and 3x3 in sudoku_ar 
    for (i in 1:9) { # for 1 rows in sudoku01
        for (j in 1:9) { # for 2 columns in sudoku01
            if (is.na(sudoku01c[i,j]) == FALSE) { # if 1 the cell in sudoku01 is non empty
                sudoku_arc[i,-j,(sudoku01c[i,j])] <- NA # delete the value from the row
                sudoku_arc[-i,j,(sudoku01c[i,j])] <- NA # delete the value from the column
                square <- sudoku_sqc[i,j] # get the number of square for the cell
                range <- get_sudoku_squares(square) # get the rowlow, row high, collow, colhigh of the square
                rangerow1 <- range[1]:range[2] # the whole range of rows of the square
                rangerow2 <- rangerow1[-(i-range[1]+1)] # the row range of square without i
                rangecol1 <- range[3]:range[4] # the whole range of columns of the square
                rangecol2 <- rangecol1[-(j-range[3]+1)] # the column range of square without j
                sudoku_arc[rangerow2,rangecol2,(sudoku01c[i,j])] <- NA # delete the value from the other cells of the square
            } # close if 1
        } # close for 2
    } # close for 1
    sudoku_ar <<- sudoku_arc # save the local copy to global variable
    assign("sudoku_ar", sudoku_ar) # assign sudoku_ar to global variable
    return(sudoku_ar) # return the cleaned sudoku_ar
} # close function


clear_sudoku01 <- function(sudoku01c, sudoku_arc) { # if the non NA length of a vector is 1 then update the original sudoku01 by singling out 
    for (i in 1:9) { # for 1 rows in sudoku_arc
        for (j in 1:9) { # for 2 columns in sudoku_arc
            sudoku_vector <- sudoku_arc[i,j,] #get the vector for the i,j cell
            if (length(sudoku_vector[is.na(sudoku_vector)==FALSE]) == 1) { # if the vector has only one non NA element
                sudoku01c[i,j] <- which(is.na(sudoku_vector)==FALSE)
            } # close if 1
        } # close for 2
    } # close for 1
    sudoku01 <<- sudoku01c # save the local copy to global variable
    assign("sudoku01", sudoku01) # assign sudoku_ar to global variable
    return(sudoku01) # return the cleaned sudoku01c
} # close function


leave_the_orphan <- function(sudoku01c, sudoku_sqc, sudoku_arc) { # single-out the vector item that is the single one in the row, column or 3x3
    for (row in 1:9) { # for 1 each row
        for (value in 1:9) { # for 2 each value in the vector
            sudoku_arc_sub <- sudoku_arc[row,,value] # select the sub vector of values in the row
            sudoku_arc_sub_col <- which(is.na(sudoku_arc_sub)==FALSE) # indices of the column of values in the row
            if (length(sudoku_arc_sub_col) == 1) { # if the value stands alone in the row
                sudoku_arc[row,sudoku_arc_sub_col,-value] <- NA # leave the value singled in the vector
            } # close if
        } # close for 2
    } # close for 1            
    sudoku_ar <<- sudoku_arc # save the local copy to global variable
    assign("sudoku_ar", sudoku_ar) # assign sudoku_ar to global variable
    return(sudoku_ar) # return the cleaned sudoku_ar
    
    for (column in 1:9) { # for 1 each column
        for (value in 1:9) { # for 2 each value in the vector
            sudoku_arc_sub <- sudoku_arc[,column,value] # select the sub vector of values in the column
            sudoku_arc_sub_row <- which(is.na(sudoku_arc_sub)==FALSE) # indices of the row of values in the column
            if (length(sudoku_arc_sub_row) == 1) { # if the value stands alone in the column
                sudoku_arc[sudoku_arc_sub_row,column,-value] <- NA # leave the value singled in the vector
            } # close if
        } # close for 2
    } # close for 1            
    sudoku_ar <<- sudoku_arc # save the local copy to global variable
    assign("sudoku_ar", sudoku_ar) # assign sudoku_ar to global variable
    return(sudoku_ar) # return the cleaned sudoku_ar
    
    for (square in 1:9) { # for 1 each square
        ranges <- (get_sudoku_squares(square)) # get the row low, row high, col low, col high indices
        for (value in 1:9) { # for 2 each value in the vector
            sudoku_arc_sub <- sudoku_arc[ranges[1]:ranges[2],ranges[3]:ranges[4],value] # select the sub matrix of values in the square
            sudoku_arc_sub_ind <- which(is.na(sudoku_arc_sub)==FALSE) # indices row first of the values in the square 
            if (length(sudoku_arc_sub_ind) == 1) { # if the value stands alone in the square
                row_sub <- (sudoku_arc_sub_ind-1) %% 3 + 1 # get the row index of the row first index of sub matrix as a vector
                col_sub <- (ceiling(sudoku_arc_sub_ind/3)) # get the column index of the row first index of sub matrix as a vector
                sudoku_arc[ranges[1] - 1 + row_sub, ranges[3] - 1 + col_sub, -value] <- NA # leave the value singled out in the vector
            } # close if
        } # close for 2
    } # close for 1
    sudoku_ar <<- sudoku_arc # save the local copy to global variable
    assign("sudoku_ar", sudoku_ar) # assign sudoku_ar to global variable
    return(sudoku_ar) # return the cleaned sudoku_ar
} # close function


clear_outside_3x3 <- function(sudoku01c, sudoku_sqc, sudoku_arc) { # clear the value that shows on only one row or columns outside the 3x3
    for (square in 1:9) { # for 1 each square
        ranges <- (get_sudoku_squares(square)) # get the row low, row high, col low, col high indices
        for (value in 1:9) { # for 2 each value in the vector
            sudoku_arc_sub <- sudoku_arc[ranges[1]:ranges[2],ranges[3]:ranges[4],value] # select the sub matrix of values in the square
            sudoku_arc_sub_ind <- which(is.na(sudoku_arc_sub)==FALSE) # indices row first of the values in the square 
            row_sub <- (sudoku_arc_sub_ind-1) %% 3 + 1 # get the row index of the row first index of sub matrix as a vector, shows in which rows of the 3x3 a certain value stands, with repeated values
            row_sub_uni <- unique(row_sub) # unique row_sub values, with no repetition 
            col_sub <- (ceiling(sudoku_arc_sub_ind/3)) # get the column index of the row first index of sub matrix as a vector, shows in which columns og the 3x3 a certain value stands, with repated values
            col_sub_uni <- unique(col_sub) # unique col_sub values, with no repetition 
            if (length(row_sub_uni) == 1) { # if 1 the value exists only in one row in a 3x3
                other_columns <- c(1:9)[-(ranges[3]:ranges[4])] # indices of the columns outside the range, other columns
                sudoku_arc[ranges[1] - 1 + row_sub_uni, other_columns, value] <- NA # erase the value in the same row outside the 3x3
            } # close if 1
            if (length(col_sub_uni) == 1) { # if 2 the value exists only in one column in a 3x3
                other_rows <- c(1:9)[-(ranges[1]:ranges[2])] # indices of the rows outside the range, other rows
                sudoku_arc[other_rows, ranges[3] - 1 + col_sub_uni, value] <- NA # erase the value in the same column outside the 3x3
            } # close if 2
        } # close for 2
    } # close for 1
    sudoku_ar <<- sudoku_arc # save the local copy to global variable
    assign("sudoku_ar", sudoku_ar) # assign sudoku_ar to global variable
    return(sudoku_ar) # return the cleaned sudoku_ar
} # close function


multiple_clear_full_row <- function(sudoku01c, sudoku_sqc, sudoku_arc) { # find common item > 1 cells in rows and delete the common items from other cells 
    for (cap in 2:4) { # for 1 max size of common items
        for (row in 1:9) { # for 2 rows for control
            indices_vector <- indices_vector_max_size(sudoku_arc, row, cap) # get the indices of cells in the row with sizes within 2 and cap
            if (length(indices_vector) >= cap) { # if 1 size of cells at least the cap 
                combinations <- t(combn(indices_vector, min(length(indices_vector),cap), FUN = NULL, simplify = TRUE)) # all combinations of cells/vectors of 2<size<cap
                n_comb  <- nrow(combinations) # number of total combinations
                for (comb in 1:n_comb) { # for 3 combinations
                    common_vec <- numeric(9) # initiate a vector to control the common items
                    common_vec[1:9] <- NA # make the common items vector of 9 NA's
                    for (elements in 1:cap) { # for 4 each element
                        vector_test <- sudoku_arc[row,combinations[comb,elements],] # get the vector for test
                        vector_test_nonna <- vector_test[is.na(vector_test)==FALSE] # nonna elements of the vector test
                        common_vec[vector_test_nonna] <- vector_test_nonna # update the common elements vector
                    } # close for 4
                    nonna_common_vec <- common_vec[is.na(common_vec)==FALSE] # nonna elements of common elements vector
                    if(length(nonna_common_vec) <= cap) { # if 2 the number of common elements is <= cap
                    sudoku_arc[row, -(combinations[comb,]), nonna_common_vec] <- NA # clean outside common item cells
                    } # close if 2
                } # close for 3
            } # close if 1
        } # close for 2
    } # close for 1
    sudoku_ar <<- sudoku_arc # update the global variable
    assign("sudoku_ar", sudoku_ar) # save sudoku_ar as global variable 
    return(sudoku_ar) # return sudoku ar
} # close function    


indices_vector_max_size <- function(sudoku_arc,row,cap) { # get the column indices of vectors with non NA elements up to a size
    vector_indices <- numeric (9) # start an empty vector for the column indices of vectors
    vector_indices[1:9] <- NA # make the vector_indices empty of length 9
    sudoku_arc_row <- sudoku_arc[row,,] # sub matrix of the row
    for (col in 1:9) { # 1 for each column
        sudoku_arc_vector <- sudoku_arc_row[col,] # vector of the cell
        nonna <- sudoku_arc_vector[is.na(sudoku_arc_vector)==FALSE] # nonna items of the vector
        size_of_nonna <- length(nonna) # number of nonna items in the vector
        if(size_of_nonna >=2 && size_of_nonna <= cap) vector_indices[col] <- col # update the indices vector with vector with sizes below cap
    } # close for 1
    vector_indices <- vector_indices[is.na(vector_indices)==FALSE] # nonna indices in the vector
    return(vector_indices) # return the vector
} # close function


multiple_clear_full_col <- function(sudoku01c, sudoku_sqc, sudoku_arc) { # find common item > 1 cells in columns and delete the common items from other cells 
    for (cap in 2:3) { # for 1 max size of common items
        for (col in 1:9) { # for 2 col for control
            indices_vector <- indices_vector_max_size2(sudoku_arc, col, cap) # get the indices of cells in the col with sizes within 2 and cap
            if (length(indices_vector) >= cap) { # if 1 size of cells at least the cap 
                combinations <- t(combn(indices_vector, min(length(indices_vector),cap), FUN = NULL, simplify = TRUE)) # all combinations of cells/vectors of 2<size<cap
                n_comb  <- nrow(combinations) # number of total combinations
                for (comb in 1:n_comb) { # for 3 combinations
                    common_vec <- numeric(9) # initiate a vector to control the common items
                    common_vec[1:9] <- NA # make the common items vector of 9 NA's
                    for (elements in 1:cap) { # for 4 each element
                        vector_test <- sudoku_arc[combinations[comb,elements],col,] # get the vector for test
                        vector_test_nonna <- vector_test[is.na(vector_test)==FALSE] # nonna elements of the vector test
                        common_vec[vector_test_nonna] <- vector_test_nonna # update the common elements vector
                    } # close for 4
                    nonna_common_vec <- common_vec[is.na(common_vec)==FALSE] # nonna elements of common elements vector
                    if(length(nonna_common_vec) <= cap) { # if 2 the number of common elements is <= cap
                    sudoku_arc[-(combinations[comb,]), col, nonna_common_vec] <- NA # clean outside common item cells
                    } # close if 2
                } # close for 3
            } # close if 1
        } # close for 2
    } # close for 1
    sudoku_ar <<- sudoku_arc # update the global variable
    assign("sudoku_ar", sudoku_ar) # save sudoku_ar as global variable 
    return(sudoku_ar) # return sudoku ar
} # close function    


indices_vector_max_size2 <- function(sudoku_arc,col,cap) { # get the row indices of vectors with non NA elements up to a size
    vector_indices <- numeric (9) # start an empty vector for the column indices of vectors
    vector_indices[1:9] <- NA # make the vector_indices empty of length 9
    sudoku_arc_col <- sudoku_arc[,col,] # sub matrix of the col
    for (row in 1:9) { # 1 for each row
        sudoku_arc_vector <- sudoku_arc_col[row,] # vector of the cell
        nonna <- sudoku_arc_vector[is.na(sudoku_arc_vector)==FALSE] # nonna items of the vector
        size_of_nonna <- length(nonna) # number of nonna items in the vector
        if(size_of_nonna >=2 && size_of_nonna <= cap) vector_indices[row] <- row # update the indices vector with vector with sizes below cap
    } # close for 1
    vector_indices <- vector_indices[is.na(vector_indices)==FALSE] # nonna indices in the vector
    return(vector_indices) # return the vector
} # close function





sudoku_squares <- function() { # define a sub function to make a matrix for numbering 3x3 squares
    sudoku_sq <- matrix (nrow = 9, ncol = 9) # create a parallel matrix for numbering each 3x3 squares
    for (i in 1:9) { # number of each square
        ranges <- get_sudoku_squares(i) # get the row low, row high, col low and col high vector for the square
        sudoku_sq[ranges[1]:ranges[2], ranges[3]:ranges[4]] <- i #rename the range with number of square
    } # close for
    return (sudoku_sq)
} # close function


get_sudoku_squares <- function(square) { # for a given square number, get the row and index indices as range
    mod3 <- (square - 1) %% 3 + 1 # modulo 3 for each square to number columns
    cei3 <- ceiling(square / 3) # ceiling of division for each sqaure to number rows
    rangerowlow <- cei3 * 3 -2 #low range of row
    rangerowhigh <- cei3 *3 #high range of row
    rangecollow <- mod3 * 3 -2 #low range of column
    rangecolhigh <- mod3 * 3 # high range of column
    ranges <- c(rangerowlow, rangerowhigh, rangecollow, rangecolhigh) # row low, row high, col low, col high vector
    return (ranges) # return the ranges
} # close function
