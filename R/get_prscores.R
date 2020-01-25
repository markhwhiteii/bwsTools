#' Get Page Rank from Adjacency Matrix
#'
#' @description 
#' Takes adjacency matrix and returns raw page rank scores.
#'
#' @param M An adjacency matrix, such as produced by get_M.
#' @param ... Arguments to igraph::page_rank
#' 
#' @return A named numeric vecor indicating page rank score.
#' @noRD
get_prscores <- function(M, ...) {
  
  # initialize all zeros, in case node is never reached ----
  out <- rep(0, ncol(M))
  names(out) <- colnames(M)
  
  # make into graph, get page rank scores ----
  g <- igraph::graph_from_adjacency_matrix(
    M, 
    mode = "directed", 
    weighted = TRUE
  )
  pr <- igraph::page_rank(g)$vector
  out[names(pr)] <- pr
  
  return(out)
}
