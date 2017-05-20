# http://www.puzzlor.com/2017-02 <- PortInAStorm.html
# Feb 2017 - Any Port in A Storm

# Any Port In A Storm

# There is significant danger to boats caught out in the open sea during a storm.  Ideally, boats will dock before the storm hits and wait it out. 

# The map above shows 20 orange boats out at sea.  With a storm approaching, each boat needs to be directed to one of three docks.
# Docks have a limited number of spaces available for boats (indicated by the rectangular spaces).
# Altogether, there are 20 boat spaces available.
# The boats are clustered into three areas and each area varies in distance to the docks (as indicated by the black arrows).
# All boats must be assigned to one space in a dock.

## Solution by Serhat Cevikel
## Assignment problem using Hungarian Algorithm with repeated destinations and source points
## Recycled from p345

library(matrixStats) # to get row and column mins easily

create.mat <- function() { # create the distance matrix from each boat to each dock space
    dock.spaces <- c(4, 7, 9) # number of places available in each dock
    cluster.sizes <- c(8, 5, 7) # size of each cluster

    mat.cl.dock <- rbind( # current distance mat. rows cluster, columns docks
                         c(1,3,2), # cluster 1
                         c(2,2,1), # cluster 2
                         c(3,2,1) # cluster 3
                         )

    sample.rows <- t(apply(mat.cl.dock, 1, rep, dock.spaces)) # create sample rows for each cluster, by repeating distances for each dock size

    ## brillant idea to repeat rows into a matrix:
    # http://stackoverflow.com/questions/15143829/r-repeat-the-values-of-a-row-in-matrix-n-number-of-times
    row.inds <- rep(1:3, cluster.sizes) # vector for repeated row indexingu
    distance.mat <- sample.rows[row.inds,] # repeat sample rows for each cluster, cluster size times into a general matrix of 20x20

}

distance.mat <- create.mat()

## RUN THE HUNGARIAN ALGORITHM. TOTALLY RECYCLED FROM P345, JUST MODIFYING THE PARAMETERS FOR THIS PROBLEM

matrix.sum <- function(mat = distance.mat, maximize = F) { ## get the max/min cost assignment, each element unique in its row and column, by hungarian algorithm
    if (maximize) mat <- -mat # if1, maximize, inverse numbers, since the algorithm minimizes

    mat.org <- mat # preserve original one, for the result
    n.row <- nrow(mat) # number of rows
    n.col <- ncol(mat) # number of cols

    # step 1.2, subtract row mins from row, col mins from cols
    row.mins <- matrixStats::rowMins(mat) # get row minimums
    mat <- mat - row.mins # subtract from matrix
    col.mins <- matrixStats::colMins(mat) # get col minimums
    mat <- t(t(mat) - col.mins) # subtract from matrix by transposing ***

    ## step 3: draw minimum number of lines, most important steps
    lines.and.zeros <- get.lines(mat, n.row) # run the get.lines function, returns the assigned zeros and covering lines
    lines.zero <- lines.and.zeros[[1]] # min number of zero covering lines. row or col (1 or 2) in the first column and row/col index in the second

    # step 4, add new zeros if independent zero assignments for all rows/columns cannot be made
    while (nrow(lines.zero) < n.row) { # while1, as long as the minimum number of zeros is less than the number of rows

        ## find the minimum uncovered element
        uncovered.mat <- mat[-(lines.zero[lines.zero[,1] == 1,2]),-(lines.zero[lines.zero[,1] == 2,2])]
        min.val <- min(uncovered.mat)

        # subtract minimum uncovered element from each uncovered row
        mat[-(lines.zero[lines.zero[,1] == 1,2]),] <- mat[-(lines.zero[lines.zero[,1] == 1,2]),] - min.val

        ## add minimum uncovered element to each covered column
        mat[,(lines.zero[lines.zero[,1] == 2,2])] <- mat[,(lines.zero[lines.zero[,1] == 2,2])] + min.val

        ## this is equal to subtracting it from all uncovered elements, adding it to all elements that are covered twice

        lines.and.zeros <- get.lines(mat, n.row) ## extract the minimum covering lines and assigned zeros as a list
        lines.zero <- lines.and.zeros[[1]] # minimum covering lines
    } # close while1


    zeros.selected <- lines.and.zeros[[2]] # get the coordinates of assigned zeros
    optimal.cost.cells <- apply(zeros.selected, 1, function(x) mat.org[x[1],x[2]]) * (1 - 2 * maximize) # get the optimal cost cells from the original matrix. invert if "maximize"
    optimal.cost <- sum(optimal.cost.cells) # sum for the optimal cost

    return(list(original.mat = mat.org, reduced.mat = mat, zero.covering.lines = lines.zero, assigned.zeros = zeros.selected, optimal.cost.cells = optimal.cost.cells, total.optimal.cost = optimal.cost))

}


# http://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x, minn = F) { # function to get the mod (or the most infrequent item in the case of minn = T
    ux <- unique(x) # get unique values
    if(!minn) {
        ux[which.max(tabulate(match(x, ux)))] # get the most frequent item
    } else {
        ux[which.min(tabulate(match(x, ux)))] # get the least frequent item
    }
}


get.lines <- function(mat, n.row) { # get the minimum number of zero covering lines and assigned zero coordinates
    zero.inds <- which(mat == 0) # vectored indices of zeros
    zero.mat <- matrix(nrow = length(zero.inds), ncol = 2) # empty mat for coordinates of zeros
    zero.mat[,1] <- (zero.inds - 1) %% n.row + 1 # row indices of zeros
    zero.mat[,2] <- ceiling(zero.inds / n.row) # col indices of zeros
    zero.mat.org <- zero.mat # keep a copy of the original zeros mat

    liness <- NULL # initiate zero covering lines matrix
    zeros.selected <- NULL # initiate assigned zeros matrix


    ### step1: assign zeros
    while(nrow(zero.mat) > 0) { # while1, the zero matrix is not exhausted
        max.val <- Mode(zero.mat[,1], minn = T) # least frequent row index

        new.zero <- zero.mat[zero.mat[,1] == max.val,,drop=F][1,] # get the newly assigned zero, without dropping matrix attribute
        zeros.selected <- rbind(zeros.selected, new.zero) # append the new zero
        zero.mat <- zero.mat[zero.mat[,1] != new.zero[1] & # delete the row and column of the new zero from the zeros matrix, w/o dropping matrix
                             zero.mat[,2] != new.zero[2],,drop=F]

    } # close while1

    rownames(zeros.selected) <- NULL # clear rownames, for better layout


    ### step2: draw minimum number of lines covering all zeros, most important step!!
    ## http://stackoverflow.com/questions/23379660/hungarian-algorithm-finding-minimum-number-of-lines-to-cover-zeroes
    ## https://math.stackexchange.com/questions/590305/finding-the-minimum-number-of-lines-to-cover-all-zeros-in-an-assignment-probem
    ## https://en.wikipedia.org/wiki/Hungarian_algorithm#Matrix <- interpretation

    row.marks <- !(1:n.row %in% zeros.selected[,1]) # s1: Mark all rows having no assignments into boolean vector
    col.marks <- rep(F, n.row) # initiate boolean vector for marking columns
    total.mark.count <- sum(row.marks) + sum(col.marks) # initiate the count of marked rows/cols, to determine stop condition
    total.mark.count.old <- 0 # initiate old count

    while (total.mark.count > total.mark.count.old) { # while2, total mark count hasn't stabilized yet
        # s2: Mark all (unmarked) columns having zeros in newly marked row(s)
        col.marks <- 1:n.row %in% zero.mat.org[zero.mat.org[,1] %in% which(row.marks),2]

        # s3: Mark all rows having assignments in newly marked columns
        new.rows.assigned <- zeros.selected[zeros.selected[,2] %in% which(col.marks),1]
        row.marks[new.rows.assigned] <- T

        ## check whether still needs repeating:
        total.mark.count.old <- total.mark.count
        total.mark.count <- sum(row.marks) + sum(col.marks) # get the new count of marked rows/cols
    } # close while2

    lines.rows <- if (sum(!row.marks) > 0) cbind(1, which(!row.marks)) else NULL # get lines on unmarked rows
    lines.cols <- if (sum(col.marks) > 0) cbind(2, which(col.marks)) else NULL # get lines on marked cols

    liness <- rbind(lines.rows, lines.cols) # combine row and col lines

    return(list(liness, zeros.selected)) # return lines and assigned zeros

}
