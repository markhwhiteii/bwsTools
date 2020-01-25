#' Perform Random Walks on Adjacency Matrix
#'
#' @description 
#' Takes adjacency matrix and returns raw random walk scores.
#'
#' @param M An adjacency matrix, such as produced by get_M.
#' @param walks Integer indicating how many random walks to simulate.
#' 
#' @return
#' A named numeric vecor indicating proportion of time a walk led to each node.
#' @noRd
get_walkscores <- function(M, walks) {
  
  # initialize all zeros, in case node is never reached ----
  out <- rep(0, ncol(M))
  names(out) <- colnames(M)
  
  # make into graph, do random walks ----
  g <- igraph::graph_from_adjacency_matrix(
    M, 
    mode = "directed", 
    weighted = TRUE
  )
  rw <- igraph::random_walk(g, 1, walks)
  rw <- prop.table(table(attr(rw, "names")))
  out[names(rw)] <- rw
  
  return(out)
}
