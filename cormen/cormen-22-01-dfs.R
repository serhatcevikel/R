# depth-first-search (dfs)
# cormen 604

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

vermat1 <- as.data.frame(cbind(1:nrow(admat), NA, NA, NA, NA)) # create a data frame to record attributes
colnames(vermat1) <- c("vertex", "color", "start", "end", "predecessor") # give column names for attribute columns

dfss <- function() { # adlist is adjacency list, vermat is the attribute matrix for vertices
    no.vertex <- nrow(vermat1) # number of vertices

    # give the attributes for non-root vertices
    for (vert in (1:no.vertex)) { # for1 across vertices
        vermat1[vert, "color"] <<- "white"
        vermat1[vert, "predecessor"] <<- NA
    } # close for1

    time1 <<- 0 # reset time

    for (vert in (1:no.vertex)) { # for2 across vertices
        if (vermat1[vert, "color"] == "white") {
            dfss.visit(vert)
        }
    }
    return(vermat1)
}

dfss.visit <- function(vert) {
    time1 <<- time1 + 1 # increment time
    vermat1[vert, "start"] <<- time1 # record start time
    vermat1[vert, "color"] <<- "gray" # record start time
    for (v in adlist1[[vert]]) { # for1, across adjacent vertices
        if (vermat1[v, "color"] == "white") {
            vermat1[v, "predecessor"] <<- vert
            dfss.visit(v)
        }
    }
    vermat1[vert, "color"] <<- "black"
    time1 <<- time1 + 1
    vermat1[vert, "end"] <<- time1
}

