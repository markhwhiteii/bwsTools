#' Transform Tidy Data into Winners and Losers Data
#' 
#' Takes tidy data and prepares it for Elo scoring in get_eloscores()
#' 
#' @param data A data.frame of the type described in details.
#' @param id A string of the name of the id column.
#' @param block A string of the name of the block column.
#' @param item A string of the name of the item column.
#' @param choice A string of the name of the choice column.
#' 
#' @return A tibble with columns of winners and losers to score.
#' 
#' @importFrom magrittr "%>%"
get_eloresults <- function(data, block, item, choice) {
  results <- dplyr::tibble() # initialize results
  
  for (b in sort(unique(data[[block]]))) {
    cb <- data[data[[block]] == b, ] # current block
    ci <- cb[[item]] # current items
    
    # find best, they beat everyone else
    best <- cb[[item]][cb[[choice]] == 1]
    
    # find neithers, they beat the worst
    neithers <- cb[[item]][cb[[choice]] == 0]
    
    # find worst, they lose to all else
    worst <- cb[[item]][cb[[choice]] == -1]
    
    # best vs. else
    results <- results %>% 
      dplyr::bind_rows(dplyr::tibble(winner = best, loser = ci[ci != best]))
    
    # neithers vs. worst
    results <- results %>% 
      dplyr::bind_rows(dplyr::tibble(winner = neithers, loser = worst))
  }
  
  return(results)
}
