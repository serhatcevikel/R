# http://www.puzzlor.com/2014-10_Fighters.html
# October 2014 - Fighters
# Four different fighters are having an all-out battle to determine who among them is the strongest.  The image above shows those four fighters: Allan, Barry, Charles, and Dan. 

# Each fighter has varying attack and health abilities.  At the start of the battle, they have differing health points: Allan has 10, Barry has 12, Charles has 16, and Dan has 18.  Also, each fighter has differing attack points: Allan has 4, Barry has 3, Charles has 2, and Dan has 1.

# The battle takes place over multiple rounds, each round consisting of a single attack.  In each round, one random attacker and one random defender are chosen.  When the attacker attacks a defender, the defender loses health points in the amount equivalent to the attacker’s attack points.  For example, if Allan is the attacker and Barry is the defender, Barry would lose 4 health points.

# The fighters continue to randomly attack and defend in subsequent rounds until there is only one fighter left, who is then declared the winner.  A fighter is removed from the battle when his life points become zero (or less).

# Question:  Which fighter is most likely to win the battle?

## Solution by Serhat Cevikel
## Using dp, memoized into a matrix, rows of which are indexed through a 4D array (4 players)
## In each recursion, all possible attack permutations are followed with equal probability
## Probabilities are joined into recursion depth through a parameter and multiplication
## Recursion stops when only one player remains with non-zero health


library(gtools) ## for non-repeated permutations


puzzlor.fighters <- function() { # get the winning probabilities of each player
    # win means only one player remains with non-zero health

    health <- c(10, 12, 16, 18) # initial health points of players a:d
    attack <- c(4, 3, 2, 1) # attack points of players a:d

    ## global objects for memoization

    # superassigned matrix to hold probabilities for each health configuration (health values of all players)
    result.mat <<- matrix(ncol = 4, nrow = prod(health + 1))
    # superassigned empty array of 4D for 4 players to hold the row index for the health configuration. dim length initial health + 1, to account for 0's. recycled from p018, p067
    result.ar <<- array(dim = health + 1, dimnames = NULL)
    # superassigned row index counter for the result.mat kept at result.ar
    row.ind <<- 1


    at.combs <- gtools::permutations(4, 2, 1:4) # permutations of players. first column attackers, second column attacked. self attack not allowed (no repetitions). recycled from p171
    results <- t(apply(at.combs, 1, fight.rec, health, attack, 1/12)) # get matrix of results through ply
    result <- colSums(results) # sum all probabilities of each player
    names(result) <- c("Allan", "Barry", "Charles", "Dan") # update player names
    return(result)

}
    

fight.rec <- function(at, health, attack, prob) { # recursive function to recalculate the win probabilities after an attack, incorporating bottom-up cumulative joint probabilities

    # at: vector of attacker and attacked no's
    # at1: attacker no
    # at2: attacked no
    # health: vector for current health levels of all players
    # attack: vector for attack points of all players
    # prob: carried joint probability

    at1 <- at[1] # attacker no
    at2 <- at[2] # attacked no

    health[at2] <- max(health[at2] - attack[at1], 0) # update the health of at2 after attack
    mat.row <- "["(result.ar, t(as.matrix(health + 1))) # get the row index for the health configuration

    if (!is.na(mat.row)) { # if1, probabilities for the health conf. already memoized

        result <- prob * result.mat[mat.row,] # get the result and multiply with the prob
        return(result)

    } else { # close if1, else1

        players <- which(health > 0) # index of alive players

        if (length(players) == 1) { # if2, only one player survives, so is the winner
            result <- rep(0,4) # vector for results
            result[players] <- prob # update the probability of the winner
            return(result)

        } else { # close if2, else2

            at.combs <- gtools::permutations(length(players), 2, players) # permutations of indices of attackers and attackeds. self attack not allowed (no repetitions in permutations) recycled from p171
            probs <- nrow(at.combs) # total count of attack permutations
            prob.new <- 1 / probs # probability of each attack permutation
            results <- t(apply(at.combs, 1, fight.rec, health, attack, prob.new)) # collect matrix of results recursing the function
            result <- colSums(results) # sum the probabilities for each player
            "["(result.ar, t(as.matrix(health + 1))) <<- row.ind # memoize the row index for result matrix into the 4d array
            result.mat[row.ind,] <<- result # memoize the results into the matrix
            row.ind <<- row.ind + 1 # increment the row index for the results matrix
            result <- prob * result # multiply with the joint probability
            return(result)

        } # close else2

    } # close else1

}


puzzlor.fighters.5D <- function() { # get the winning probabilities of each player
    # win means only one player remains with non-zero health

    health <- c(10, 12, 16, 18) # initial health points of players a:d
    attack <- c(4, 3, 2, 1) # attack points of players a:d

    ## global objects for memoization

    # superassigned matrix to hold probabilities for each health configuration (health values of all players)
    #result.mat <<- matrix(ncol = 4, nrow = prod(health + 1))
    # superassigned empty array of 4D for 4 players to hold the row index for the health configuration. dim length initial health + 1, to account for 0's. recycled from p018, p067
    result.ar <<- array(dim = c(health + 1, 4), dimnames = NULL)
    # superassigned row index counter for the result.mat kept at result.ar
    #row.ind <<- 1


    at.combs <- gtools::permutations(4, 2, 1:4) # permutations of players. first column attackers, second column attacked. self attack not allowed (no repetitions). recycled from p171
    results <- t(apply(at.combs, 1, fight.rec.5D, health, attack, 1/12)) # get matrix of results through ply
    result <- colSums(results) # sum all probabilities of each player
    names(result) <- c("Allan", "Barry", "Charles", "Dan") # update player names
    return(result)

}
    

fight.rec.5D <- function(at, health, attack, prob) { # recursive function to recalculate the win probabilities after an attack, incorporating bottom-up cumulative joint probabilities

    # at: vector of attacker and attacked no's
    # at1: attacker no
    # at2: attacked no
    # health: vector for current health levels of all players
    # attack: vector for attack points of all players
    # prob: carried joint probability

    at1 <- at[1] # attacker no
    at2 <- at[2] # attacked no

    health[at2] <- max(health[at2] - attack[at1], 0) # update the health of at2 after attack
    mat.index <- cbind(matrix(rep(health + 1, 4), nrow = 4, byrow = T), 1:4)

    mat.row <- "["(result.ar, mat.index) # get the memoized health configuration

    if (!is.na(mat.row[1])) { # if1, probabilities for the health conf. already memoized

        result <- prob * mat.row # get the result and multiply with the prob
        return(result)

    } else { # close if1, else1

        players <- which(health > 0) # index of alive players

        if (length(players) == 1) { # if2, only one player survives, so is the winner
            result <- rep(0,4) # vector for results
            result[players] <- prob # update the probability of the winner
            return(result)

        } else { # close if2, else2

            at.combs <- gtools::permutations(length(players), 2, players) # permutations of indices of attackers and attackeds. self attack not allowed (no repetitions in permutations) recycled from p171
            probs <- nrow(at.combs) # total count of attack permutations
            prob.new <- 1 / probs # probability of each attack permutation
            results <- t(apply(at.combs, 1, fight.rec.5D, health, attack, prob.new)) # collect matrix of results recursing the function
            result <- colSums(results) # sum the probabilities for each player
            "["(result.ar, mat.index) <<- result # memoize the health configuration
            result <- prob * result # multiply with the joint probability
            return(result)

        } # close else2

    } # close else1

}

### non memoized version
puzzlor.fighters.nm <- function() { # get the winning probabilities of each player
    # win means only one player remains with non-zero health

    health <- c(10, 12, 16, 18) # initial health points of players a:d
    attack <- c(4, 3, 2, 1) # attack points of players a:d

    at.combs <- gtools::permutations(4, 2, 1:4) # permutations of players. first column attackers, second column attacked. self attack not allowed (no repetitions). recycled from p171
    results <- t(apply(at.combs, 1, fight.rec.nm, health, attack, 1/12)) # get matrix of results through ply
    result <- colSums(results) # sum all probabilities of each player
    names(result) <- c("Allan", "Barry", "Charles", "Dan") # update player names
    return(result)

}
    

fight.rec.nm <- function(at, health, attack, prob) { # recursive function to recalculate the win probabilities after an attack, incorporating bottom-up cumulative joint probabilities

    # at: vector of attacker and attacked no's
    # at1: attacker no
    # at2: attacked no
    # health: vector for current health levels of all players
    # attack: vector for attack points of all players
    # prob: carried joint probability

    at1 <- at[1] # attacker no
    at2 <- at[2] # attacked no

    health[at2] <- max(health[at2] - attack[at1], 0) # update the health of at2 after attack

    players <- which(health > 0) # index of alive players

    if (length(players) == 1) { # if2, only one player survives, so is the winner
        result <- rep(0,4) # vector for results
        result[players] <- prob # update the probability of the winner
        return(result)

    } else { # close if2, else2

        at.combs <- gtools::permutations(length(players), 2, players) # permutations of indices of attackers and attackeds. self attack not allowed (no repetitions in permutations) recycled from p171
        probs <- nrow(at.combs) # total count of attack permutations
        prob.new <- 1 / probs # probability of each attack permutation
        results <- t(apply(at.combs, 1, fight.rec.nm, health, attack, prob.new)) # collect matrix of results recursing the function
        result <- colSums(results) # sum the probabilities for each player
        #"["(result.ar, t(as.matrix(health + 1))) <<- row.ind # memoize the row index for result matrix into the 4d array
        #result.mat[row.ind,] <<- result # memoize the results into the matrix
        #row.ind <<- row.ind + 1 # increment the row index for the results matrix
        result <- prob * result # multiply with the joint probability
        return(result)

    } # close else2

}
