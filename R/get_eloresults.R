#' Transform Tidy Data into Winners and Losers Data
#' 
#' Takes tidy data and prepares it for Elo scoring in get_eloscores()
#' 
#' @param data A data.frame of the type described in details.
#' @param block A string of the name of the block column.
#' @param item A string of the name of the item column.
#' @param choice A string of the name of the choice column.
#' @param aggregate A logical indicating if this is for aggregate scores.
#' @param id A string indicating the name of the id column. Not necessary for
#'   individual-level scores and is ignored.
#' @return A tibble with columns of winners and losers to score.
get_eloresults <- function(data, block, item, choice, aggregate = FALSE,
                           id = NULL) {
  results <- dplyr::tibble() # initialize results
  
  if (aggregate) {
    data[[block]] <- paste(data[[id]], data[[block]], sep = "_") # recode block
  }
  
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
    results <- dplyr::bind_rows(
      results, 
      dplyr::tibble(winner = best, loser = ci[ci != best])
    )
    
    # neithers vs. worst
    results <- dplyr::bind_rows(
      results, 
      dplyr::tibble(winner = neithers, loser = worst)
    )
  }
  
  # make dummies
  # nonsense names so no user accidentally has the same issue name
  results <- dplyr::bind_rows(
    dplyr::tibble(winner = "1839winner!^dummyy", loser = unique(data[[item]])),
    dplyr::tibble(winner = unique(data[[item]]), loser = "1839loserdummyy!^")
  ) %>% 
    dplyr::bind_rows(results)
  
  return(results)
}
