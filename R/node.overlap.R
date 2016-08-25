#' Takes an overlap matrix and a tree and returns average overlap at nodes using FT averaging method
#'
#' @param overlap
#' @param tree
#'
#' @export node.overlap


node.overlap <- function(overlap, tree, usebrlens = FALSE){
  
  # Test to make sure our data aren't crap
  if(any(!rownames(overlap) %in% colnames(overlap))){
    stop("Row and column names do not match!")
  }
  
  if(!inherits(tree, "phylo")){
    stop("Tree is not a phylo object!")
  }
  
  if(any(!rownames(overlap) %in% tree$tip.label)){
    stop("Overlap matrix names and tip labels do not match!")
  }
  
  branchcomps <- NA
  
  # Get numbers for internal nodes
  nodes <- unique(tree$edge[,1])
  
  if(usebrlens == TRUE){
    # Test that they exist
    
    
    # Figure out how many comparisons will be made across each internal branch
    
    # Number of comparisons made across each branch
    
    
    
    
    
    branchcomps <- sapply(tree$edge[,2], 
                          function(x) length(descendants(tree, node = x)))
    branchcomps <- 1/(branchcomps * (length(tree$tip.label) - branchcomps))
  }
  
  # Return a table of node numbers and scaled overlap values
  return(cbind(nodes,
               sapply(nodes, function(x) single.node.overlap(x, overlap, tree, usebrlens, branchcomps))))
}

# Number of comparisons across a node
num.comps <- function(node, tree){
  daughters <- tree$edge[tree$edge[, 1] == node, 2]
  tip.counts <- sapply(daughters, function(x) length(descendants(tree, x)))
  comps <- t(combn(tip.counts, 2))
  totalcomps <- sum(apply(comps, 1, prod))
  return(totalcomps)
}

# This function takes an internal node number, overlap matrix, and tree
# and calculates the scaled overlap using the FT method for all pairs of 
# daughter nodes
single.node.overlap <- function(node, overlap, tree, usebrlens, branchcomps){
  
  # Initialize node overlap
  this.node.overlap <- 0
  
  # Get immediate descendants of node
  daughters <- tree$edge[tree$edge[, 1] == node, 2]
  
  # Get all combinations of daughters into table
  daughter.comparisons <- t(combn(daughters, 2))
  
  # Get overlap for each pair of daughter clades
  daughter.overlaps <- apply(daughter.comparisons, 1, function(x) 
    get.daughter.overlap(tree, overlap, x, usebrlens, branchcomps))
  
  # Add it up 
  this.node.overlap <- sum(daughter.overlaps)
  
  # Bottom part of the B&N 2005 equation - divide by sum of brlens
  if(usebrlens == TRUE){
    whichbrlens <- c(descendants(tree, node, internal = TRUE), node)
    sumbrlens <- sum(tree$edge.length[which(apply(tree$edge, 
                                              1, function(x) all(x %in% whichbrlens)))])
    
    this.node.overlap <- this.node.overlap/sumbrlens
  }
  
  return(this.node.overlap)
}

# Get the scaled overlap for a single pair of daughters
get.daughter.overlap <- function(tree, overlap, nodes, usebrlens, branchcomps){
  
  clade1 <- descendants(tree, nodes[1])
  clade2 <- descendants(tree, nodes[2])
  
  comparisons <- expand.grid(clade1, clade2)
  
  if(usebrlens == TRUE){
    mults <- apply(comparisons, 1, function(x) get.mult.brlens(tree, as.numeric(x), branchcomps))
  } else {
    mults <- apply(comparisons, 1, function(x) get.mult(tree, as.numeric(x))) 
  }
  
  raw.overlaps <- apply(comparisons, 1, function(x) overlap[tree$tip.label[x[1]], tree$tip.label[x[2]]])
  
  return(sum(mults * raw.overlaps))
}

# This function takes two tips and calculates the multiplier needed for their
# overlap value.  Function is a heavily modified version of the one from
# phyloclim.
get.mult <- function (tree, tips){
  
  ntips <- length(tree$tip.label)
  mrca <- getMRCA(tree, tips)
  nds <- descendants(tree, mrca, internal = TRUE)
  
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
    check <- function(x, tips) any(tips %in% descendants(tree, x))
    id <- sapply(nds, check, tips = tips)
    mult <- 1/prod(daughters[id]) * 1/(choose(length(which(tree$edge[,1] == mrca)), 2))
  }
  return(mult)
}




# This function takes two tips and calculates the multiplier needed for their
# overlap value using the method of Bolnick and Near that incorporates
# branch lengths.
get.mult.brlens <- function (tree, tips, branchcomps){
  
  ntips <- length(tree$tip.label)
  mrca <- getMRCA(tree, tips)
  
  # Get all nodes descending from mrca, throw into vector with mrca
  nds <- c(descendants(tree, mrca, internal = TRUE), mrca)
  
  # Get only those edges leading to the tips of interest
  check <- function(x, tips) any(tips %in% descendants(tree, x))
  id <- sapply(nds, check, tips = tips)
  
  # Figure out the indices in the edge matrix for those edges
  connectors <- which(apply(tree$edge, 1, function(x) all(x %in% nds[id])))
  
  # Calculate multiplier as in B&N 2005
  mult <- sum(tree$edge.length[connectors] * branchcomps[connectors])
  
  print(mult)
  
  return(mult)
}
