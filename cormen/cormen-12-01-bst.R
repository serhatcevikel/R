# binary search tree
# cormen 288

library(igraph)

bst1 <- read.csv("binary.search.tree.01.csv")
bst2 <- read.csv("binary.search.tree.02.csv")
bst3 <- read.csv("binary.search.tree.03.csv")

plot.tree <- function(bst = bst1) { # plot bst
    root <- which(is.na(bst$Parent)) # find the root (parent is NA)
    links1 <- apply(bst, 1, bstlinks) # create children links as list
    links2 <- do.call(rbind, links1) # convert list to matrix
    nodes <- bst[,c(1,5)] # get node no's and keys
    net <- graph_from_data_frame(d = links2, vertices = nodes, directed = F) # network object
    plot(net,
         layout = layout.reingold.tilford(net, root=root),
         vertex.label = paste(nodes[,1], nodes[,2], sep = "/")
         ) # plot as tree with root
    return(links2)
} # close funtion

bstlinks <- function(bst) { # convert children nodes to links
    ch <- which(!is.na(bst[2:3])) # get existing children
    no.ch <- length(ch) # number of children
    if (no.ch > 0) { # if children exists
        bstmat <- cbind(rep(bst[1], no.ch), bst[1 + ch]) # create a matrix for links
        return(bstmat)
    } # close if
} # close function


# pg 288
inorder.tree.walk <- function(bst = bst1, x = 0, root = F) { # return the keys of bst in sorted form
    if (!is.na(x)) { # if1, x is not nil
        
        if (x == 0) { # if2, no x is provided and is 0
            root <- T # toggle root flag to T
            walk <<- NULL # initiate superassigned walk vector
            x <- which(is.na(bst$Parent)) # find the root (parent is NA)
        } # close if2

        inorder.tree.walk(bst, bst$Left[x], F) # recurse the left tree (lower values)
        walk <<- c(walk, (bst$Key[x])) # append the current key
        inorder.tree.walk(bst, bst$Right[x], F) # recurse the right tree (upper values)

    } # close if1

    if (root) return(walk) # if at the root, return the walk vector

} # close function
    


# pg 290
bst.tree.search <- function(bst = bst1, node = 0, key = 5) { # return the node of the key value
    if (!is.na(node)) { # if1, x is not nil
        if (node == 0) { # if1, no node is provided and is 0
            node <- which(is.na(bst$Parent)) # find the root (parent is NA)
        } # close if1
    }

    if (is.na(node) | key == bst$Key[node]) { # if2, arrived at the node or does not exist
        return(node) # return the node and stop
    } # close if2

    if (key < bst$Key[node]) { # if3, searched key is smaller than the key of current node
        bst.tree.search(bst, bst$Left[node], key) # recurse at the left children
    } else { # close if3, else3
        bst.tree.search(bst, bst$Right[node], key) # recurse at the right children
    } # close else3

} # close function


# pg 291
iter.bst.tree.search <- function(bst = bst3, node = 0, key = 9) { # return the node of the key value
    if (!is.na(node)) { # if1, x is not nil
        if (node == 0) { # if1, no node is provided and is 0
            node <- which(is.na(bst$Parent)) # find the root (parent is NA)
        } # close if1
    }

    while (!is.na(node) & key != bst$Key[node]) { # while value is not reached and node exists
        if (key < bst$Key[node]) { # if3, searched key is smaller than the key of current node
            node <- bst$Left[node] # iterate node to the left children
        } else { # close if3, else3
            node <- bst$Right[node] # iterate node to the left children
        } # close else
    } # close while

    return(node)

}


# pg 291
bst.tree.minimum <- function(bst = bst3, node = 0) { # return the node and value of minimum key
    if (!is.na(node)) { # if1, x is not nil
        if (node == 0) { # if1, no node is provided and is 0
            node <- which(is.na(bst$Parent)) # find the root (parent is NA)
        } # close if1
    }

    while (!is.na(bst$Left[node])) { # while left child exists
        node <- bst$Left[node] # iterate node to the left children
    } # close while

    return(list(node, bst$Key[node]))

}



# pg 291
bst.tree.maximum <- function(bst = bst3, node = 0) { # return the node and value of maximum key
    if (!is.na(node)) { # if1, x is not nil
        if (node == 0) { # if1, no node is provided and is 0
            node <- which(is.na(bst$Parent)) # find the root (parent is NA)
        } # close if1
    }

    while (!is.na(bst$Right[node])) { # while left child exists
        node <- bst$Right[node] # iterate node to the left children
    } # close while

    return(list(node, bst$Key[node]))

}


# pg 292
bst.tree.successor <- function(bst = bst3, node = 2) { # find the successor node with the smallest key greater than x
    if (!is.na(bst$Right[node])) { # if1, right child exists
        bst.tree.minimum(bst, bst$Right[node]) # return the minimum starting from right child
    } else { # close if1, else1
        parnt <- bst$Parent[node] # parent node

        while (!is.na(parnt) & node == if (is.na(bst$Right[parnt])) 0 else bst$Right[parnt]) { # while, parent exists and current node is a right child (so node key is larger than parent key)
            node <- parnt # make current parrent new node
            parnt <- bst$Parent[parnt] # move the parent up
        } # close while1

    return(list(parnt, bst$Key[parnt]))
    } # close else1
}



# pg 293
bst.tree.predecessor <- function(bst = bst3, node = 2) { # find the predecessor node with the largest key smaller than x
    if (!is.na(bst$Left[node])) { # if1, left child exists
        bst.tree.maximum(bst, bst$Left[node]) # return the maximum starting from left child
    } else { # close if1, else1
        parnt <- bst$Parent[node] # parent node

        while (!is.na(parnt) & node == if (is.na(bst$Left[parnt])) 0 else bst$Left[parnt]) { # while, parent exists and current node is a left child (so node key is smaller than parent key)
            node <- parnt # make current parrent new node
            parnt <- bst$Parent[parnt] # move the parent up
        } # close while1

    return(list(parnt, bst$Key[parnt]))
    } # close else1
}



# pg 294
bst.tree.insert <- function(bst = bst3, key = 25) { # return the node and value of maximum key

    par(mfrow=c(1,2)) # split screen for 2 plots
    plot.tree(bst) # plot tree before insertion

    root <- which(is.na(bst$Parent)) # find the root (parent is NA)
    
    y <- NA # tentative parent
    x <- root # starting point

    while (!is.na(x)) { # while1, unless a leaf is reached
        y <- x # make current node a tentative parent
        if (key < bst$Key[x]) { # if1, value is smaller than the key of current node
            x <- bst$Left[x] # make the left child the current node
        } else { # close if1
            x <- bst$Right[x]
        } # close else1
    } # close while1

    node <- max(bst$Node) + 1 # new node number
    newrow <- c(node, NA, NA, y, key) # new row. last y node is the parent
    bst <- rbind(bst, newrow) # append the row

    # if y is nil (NA), new node is the root

    if (!is.na(y)) { # if parent is not nil
        if (key < bst$Key[y]) { # key is smaller than that of the parent
            bst$Left[y] <- node # update the left child of parent to the node
        } else {
            bst$Right[y] <- node # update the right child of parent to the node
        } # close else
    } # close if

    plot.tree(bst) # plot tree after insertion

    return(bst)

}
