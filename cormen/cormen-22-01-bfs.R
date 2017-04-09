# breadth-first-search (bfs)
# cormen 595

library(igraph)

links <- as.data.frame(cbind(c(1,1,2,3,4,4,5), c(2,3,1,4,5,1,3))) # edges as a data frame
nodes <- as.data.frame(cbind(c(1,2,3,4,5), c(10,10,10,20,20))) # vertices as a data frame

net <- graph_from_data_frame(d = links, vertices = nodes, directed = F) # network object
net2 <- simplify(net, remove.multiple = T, remove.loops = F) # simplify the network object
plot(net2) # plot the graph

admat <- as.matrix(read.csv("adjacency.matrix.csv", header = F)) # read adjacency matrix from csv
admat <- (admat + t(admat) > 0) + 0 # make it symmetric
net3 <- graph_from_adjacency_matrix(admat, mode = "undirected") # read into network object
net4 <- simplify(net3, remove.multiple = T, remove.loops = F) # simplify the network object
plot(net4) # plot the graph

adlist1 <- apply(admat, 1, function(x) which(x == 1)) # extract adjacency list from adjacency matrix

vermat1 <- as.data.frame(cbind(1:nrow(admat), NA, NA, NA)) # create a data frame to record attributes
colnames(vermat1) <- c("vertex", "color", "distance", "predecessor") # give column names for attribute columns

bfss <- function(root = 1, adlist = adlist1, vermat = vermat1) { # root is root node no, adlist is adjacency list, vermat is the attribute matrix for vertices
    no.vertex <- nrow(vermat) # number of vertices

    # give the attributes for non-root vertices
    for (vert in (1:no.vertex)[-root]) { # for1 across non root vertices
        vermat[vert, "color"] <- "white"
        vermat[vert, "distance"] <- Inf
        vermat[vert, "predecessor"] <- NA
    } # close for1

    ## attributes for root
    vermat[root, "color"] <- "gray"
    vermat[root, "distance"] <- 0
    vermat[root, "predecessor"] <- NA

    Qu <<- NULL # initiate queue 
    
    enqueue(root) # add root to queue

    while (length(Qu) > 0) { # while1 queue Q is not empty
        u <- dequeue() # dequeue Q and return head

        for (v in adlist[[u]]) { # for2 each adjacent vertice in u entry of list

            if (vermat[v, "color"] == "white") { # if1 vertice was not examined yet
                vermat[v, "color"] <- "gray" # make it gray - in process
                vermat[v, "distance"] <- vermat[u, "distance"] + 1 # increment its distance to root
                vermat[v, "predecessor"] <- u # record its predecessor
                enqueue(v) # enqueue v so that its adjacent nodes will be examined
            } # close if1

        } # close for2

        vermat[u, "color"] <- "black" # u is finished for examination, all adjacent vertices has been covered

    } # close while1
    par(mfrow=c(1,2)) # split screen for 2 plots

    plot(net4, vertex.label = paste(vermat[,1], vermat[,3], sep = "/")) # plot all edges with distances to root at vertices

    links <- vermat[-root,c(1,4)] # prepare plot for tree (vertices and predecessors)
    nodes <- vermat[,c(1,3)] # vertices and attributes

    net <- graph_from_data_frame(d = links, vertices = nodes, directed = F) # create new network object as a tree
    net2 <- simplify(net, remove.multiple = T, remove.loops = F) # simplify the object
    plot(net2, vertex.label = paste(nodes[,1], nodes[,2], sep = "/"), add = F) # plot tree with distances to root

    vermat2 <<- vermat 

    return(vermat) # return the distances and predecessors df

} # close function


# cormen pg 601
printpath <- function(vermat = vermat2, path1 = NULL, v = 10) { # using vermat vertice matrix, print the path from v to s
    s <- which(vermat[,3] == 0) # find the root
    predecessor <- vermat[v,4] # predecessor vertice of v
    if (v == s) { # if current v is root s
        return(c(path1, s)) # append it to incoming path and return
    } else if (is.na(vermat[v,4])) { # else, if predecessor is NIL (other than the root)
            return("no.path") # print no path
    } else  printpath(vermat, c(path1, v), predecessor) # else append current v to existing path and recurse
} # close function


enqueue <- function(vert) Qu <<- c(Qu, vert) # add vert to the end of queue Q

dequeue <- function() { # delete the first item of queue Q and return it
    headq <- Qu[1]
    Qu <<- Qu[-1]
    return(headq)
}
