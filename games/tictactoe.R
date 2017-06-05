# brute force tictactoe optimizer
# machine selects the best move considering the best move of the opponent in subsequent moves


ttt <- function(parallel = F, starts = 1) # 1 is real, 2 is machine
{
    move <- 1 # initiate move

    tab <- matrix(as.integer(), nrow = 3, ncol = 3) # initiate the table

    player <- starts # first move

    while (move < 10) # as long as there are moves left
    {
        if (player == 1) # if real player
        {
            tab <- dataen(tab) # data entry
        }
        else
        {
            if (move == 1) # if the first move
            {
                best.move <- sample(1:9, 1) # randomly select
                tab[best.move] <- player # update the table
            }
            else
            {
                best.move <- play(tab, player, move, parallel) # recursively find the best move
                tab[best.move[2]] <- player # update the table
            }
        }

        winner <- check.win(tab) # check the winner

        if(is.na(winner)) { # if no winner yet
            player <- 3 - player # switch players
            move <- move + 1 # increment moves
        }
        else
        {
            print(sprintf("winner is %s", winner)) # announce the winner
            return(tab) # return the table
        }
    }

    print("tie") # announce the tie
    return(tab) # return the table

}


play <- function(tab, player, move, parallel) # get the optimal move
{
    if (move == 9) #if1, last move
    {
        empty <- which(is.na(tab)) # index of empty cell
        tab[empty] <- player # last move
        winner <- check.win(tab) # check the winner
        return(c(winner, empty)) # return the winner and move pair
    }
    else
    {
        empty <- which(is.na(tab)) # index of empty cells
        if (!parallel | move > 2) # if not parallel or move more than 2
        {
            # return a matrix: 1st column is winners, 2nd column is optimum moves
            results.mat <- t(sapply(  empty,# recursively apply the optimum move function
                                function(x, tab)
                                    {
                                        tab[x] <- player # update the table
                                        winner <- check.win(tab) # check the winner
                                        if (!is.na(winner)) # if there is a winner
                                        {
                                            return(c(winner,x)) # return the winner, move pair
                                        }
                                        else
                                        {
                                        # recurse the play function and return the pair
                                        result <- play(tab, 3 - player, move + 1, parallel)
                                        # substitute the current move into the optimum move value
                                        return(c(result[1], x))
                                        }
                                    },
                                tab) )
        }
        else
        {
            results.mat <- t(parallel::mcmapply( # parallel version of the above function
                                function(x, tab)
                                    {
                                        tab[x] <- player
                                        winner <- check.win(tab)
                                        if (!is.na(winner))
                                        {
                                            return(c(winner,x))
                                        }
                                        else
                                        {
                                        result <- play(tab, 3 - player, move + 1, parallel)
                                        return(c(result[1], x))
                                        }
                                    },
                                    empty,
                                list(tab), mc.cores = 8) )
        }


        results <- results.mat[,1] # get the winners for each current move

        if (any(results == player, na.rm =T)) # if the current player wins any
        {
            moves <- which(results == player & !is.na(results)) # get winning moves
        }
        else
        {
            if(any(is.na(results))) # if there is a tie
            {
                moves <- which(is.na(results)) # get tie moves
            }
            else
            {
               moves <- 1:length(empty) # nothing to do, select a losing move
            }
        }

        if (length(moves) == 1) # if only one move
        {
            return(results.mat[moves,]) # return the eventual result (not the current)
        }
        else
        {
            move <- sample(moves, 1) # select a move among alternatives
            return(results.mat[move,]) # return the eventual result (not the current)
        }

    }

}


dataen <- function(tab) # enter move interactively, for 1st player only
{
    tab <- fix(tab) # get value and update table
    return(tab)
}


check.win <- function(tab) # check whether there is any win
{
    wins <- c( # apply the check rule for rows, columns and diagonals
                apply(tab, 1, check.win.2),
                apply(tab, 2, check.win.2),
                check.win.2(diag(tab)),
                check.win.2(tab[c(3,5,7)])
                )

    if(any(!is.na(wins))) # if any win exists
    {
        value <- wins[!is.na(wins)][1] # get the winner
        return(value)
    } else {
        return(NA) # else return NA which means no definite winner yet
    }

}


check.win.2 <- function(vec) # check whether the triple has a winner
{
    if(all(vec == vec[1], na.rm = T) & !any(is.na(vec))) # all three are full and the same
    {
        return(vec[1]) # return the winner
    } else {
        return(NA) # else return NA, no winner yet
    }
}
