# Rock paper scissors game against PC
# For CMPE140

# rock: 1 scissors: 2 paper: 3

# gamerps is the main function

gamerps <- function(scoremax = 5) {
    player <- cbind(c("PC", "wins"), c("You", "win")) # name of players. PC: 1, You: 2
    tools <- c("rock", "scissors", "paper") # tools
    scores <- c(0, 0) # scores for PC and you
    move <- c(0, 0) # current moves

    while (max(scores) < scoremax) { # while1, as long as max score unreached
        move[1] <- sample(1:3, 1) # random move for pc
        move[2] <- as.integer(readline(prompt = "Rock: 1, Scissors: 2, Paper:3 >")) # move input by you
        valid <- move[2] %in% 1:3 # answer is one of 1:3

        while(!valid) { # while2, input is not valid
            cat("Input NOT Valid\n") # warning
            move[2] <- as.integer(readline(prompt = "Rock: 1, Scissors: 2, Paper:3 >")) # repeat the input by you
            valid <- move[2] %in% 1:3 # answer is one of 1:3
        } # close while2

        winn <- (move[2] - move[1]) %% 3 # index of winner. HEART OF THE CODE
        cat(sprintf("PC chose the %s, you chose the %s\n", tools[move[1]], tools[move[2]])) # print the move's tools

        if (winn == 0) cat("Round tied\n") else { # if 0 tie else
            scores[winn] <- scores[winn] + 1 # increment winner score
            cat(sprintf("%s %s the round\n", player[1, winn], player[2, winn])) # print the winner of round
        } # close if

        cat(sprintf("PC's score: %s, your score: %s\n\n", scores[1], scores[2])) # print the scores

    } # close while1

    winner <- which(scores == scoremax)
    #cat(sprintf("PC's score: %s, your score: %s\n", scores[1], scores[2])) # print the scores
    cat(sprintf("%s %s the game\n", player[1, winner], player[2, winner])) # print the winner of the game

}


rps <- function(x, y) { # x and y chooses among 1:3, returns the index of winner or "0" if tie
    return((y - x) %% 3) # who wins, 0: tie, 1: x, 2: y
}

rpsall <- function() { # return all possibilities. 0 means tie, 1 means "1st player (rows) wins", 2 means "2nd player (columns) wins
    outer(1:3, 1:3, rps)
}

