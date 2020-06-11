#' Takes an overlap matrix and a tree and returns average overlap at nodes using Fitzpatrick & Turelli averaging method.  Typicall called via enmtools.aoc rather than used directly.
#'
#' @param overlap An overlap matrix
#' @param tree A tree
#'
#' @return A data frame of node ages and overlaps


node.overlap <- function(overlap, tree){

   check.packages("ape")

   # Get numbers for internal nodes
   nodes <- ape::branching.times(tree)
   tree$node.label <- NULL

   # Return a table of node numbers and scaled overlap values
   output <- data.frame(cbind(nodes,
                sapply(names(nodes), function(x) single.node.overlap(as.numeric(x), overlap, tree))))
   colnames(output) <- c("age", "overlap")
   return(output)
}


get.descendant.tips <- function(tree, node, internal = FALSE) {

  daughters <- tree$edge[ , 2][tree$edge[ , 1] == node]
  old_node_list <- vector()
  new_node_list <- c(old_node_list, daughters)

  while(!all(new_node_list %in% old_node_list)) {
    old_node_list <- new_node_list
    new_node_list <- unique(c(new_node_list, tree$edge[ , 2][tree$edge[ , 1] %in% new_node_list]))
  }
  if (length(new_node_list) == 0) {
    new_node_list <- node
  }
  if(internal) {
    return(sort(new_node_list))
  } else {
    return(sort(new_node_list[new_node_list <= length(tree$tip.label)]))
  }

}


# This function takes an internal node number, overlap matrix, and tree
# and calculates the scaled overlap using the FT method for all pairs of
# daughter nodes
single.node.overlap <- function(node, overlap, tree){

   # Initialize node overlap
   this.node.overlap <- 0

   # Get immediate descendants of node
   daughters <- tree$edge[tree$edge[, 1] == node, 2]

   # Get all combinations of daughters into table
   daughter.comparisons <- t(combn(daughters, 2))

   # Get overlap for each pair of daughter clades
   daughter.overlaps <- apply(daughter.comparisons, 1, function(x)
      get.daughter.overlap(tree, overlap, x))

   # Add it up and return it
   this.node.overlap <- sum(daughter.overlaps)

   return(this.node.overlap)
}

# This is no longer exported by phyloclim, so I've just copied it over
# for now.
descendants <- function(tree, node, internal = FALSE, string = FALSE){

    tips <- seq(along = tree$tip.label)
    x <- tree$edge[,2][tree$edge[,1] == node]
    repeat{
      xx <- x
      x <- sort(unique(c(x, tree$edge[,2][tree$edge[,1] %in% x])))
      if (identical(x, xx)) break
    }
    # return tip number if input is tip number:
    # -----------------------------------------
    if (length(x) == 0) x <- node
    if (!internal)
      x <- x[x %in% tips]
    if (string)
      x <- tree$tip.label[x]
    x
  }

# Get the scaled overlap for a single pair of daughters
get.daughter.overlap <- function(tree, overlap, nodes){

   clade1 <- get.descendant.tips(tree, nodes[1])
   clade2 <- get.descendant.tips(tree, nodes[2])

   comparisons <- expand.grid(clade1, clade2)

   raw.overlaps <- unlist(apply(comparisons, 1, function(x) overlap[tree$tip.label[x[1]], tree$tip.label[x[2]]]))

   mults <- apply(comparisons, 1, function(x) get.mult(tree, as.numeric(x)))

   return(sum(mults * raw.overlaps))
}

# This function takes two tips and calculates the multiplier needed for their
# overlap value.  Function is a heavily modified version of the one from
# phyloclim.
get.mult <- function (tree, tips){

   check.packages("ape")

   ntips <- length(tree$tip.label)
   mrca <- ape::getMRCA(tree, tips)
   nds <- get.descendant.tips(tree, mrca, internal = TRUE)

   if (identical(sort(as.integer(nds)), sort(as.integer(tips)))){

      # Clade is a terminal sister pair
      mult <- 1

   } else if (all(tree$edge[which(tree$edge[,2] %in% tips), 1] == tree$edge[which(tree$edge[,2] %in% tips), 1][1])){

      # Clade includes both terminal taxa but is a polytomy
      mult <- 1/(choose(length(which(tree$edge[,1] == mrca)), 2))

   } else {
      # Not a polytomy, not a sister pair

      # Get internal nodes
      nds <- nds[nds > ntips]

      # Figure out how many daughters each node has
      daughters <- sapply(nds, function(x) length(which(tree$edge[,1] == x)))

      # Trim it down to the number of daughters per node that falls along path between tips
      check <- function(x, tips) any(tips %in% get.descendant.tips(tree, x))
      id <- sapply(nds, check, tips = tips)
      mult <- 1/prod(daughters[id]) * 1/(choose(length(which(tree$edge[,1] == mrca)), 2))
   }
   return(mult)
}

