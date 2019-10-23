#' Turn Win-Loss Record Data into Elo Scores
#' 
#' Takes what get_eloresults() returns and turns these into Elo scores.
#' 
#' @param eloresults What get_eloresults() returns
#' @param K The Elo K-factor. The default is 30, per Hollis (2018).
#' @param iter Number of different randomizations of the "matchup" order to 
#'   iterate through.
#'   
#' @return A tibble with columns for the item and Elo score
#'
#' @importFrom magrittr "%>%"
get_eloscores <- function(eloresults, K = 30, iter = 150) {
  out <- dplyr::tibble() # initialize output
  
  for (z in seq_len(iter)) {
    # initialzie elos
    elos <- dplyr::tibble(item = unique(unlist(eloresults)), elo = 1000)
    
    # randomize order
    eloresults <- eloresults[sample(nrow(eloresults)), ]
    
    for (i in seq_len(nrow(eloresults))) {
      # get elo of winner and elo of loser
      winner_elo <- elos$elo[elos$item == eloresults$winner[i]]
      loser_elo <- elos$elo[elos$item == eloresults$loser[i]]
      
      # hollis (2018), equation 2
      Qw <- 10 ^ (winner_elo / 400)
      Ql <- 10 ^ (loser_elo / 400)
      Ew <- Qw / (Qw + Ql)
      
      # hollis (2018), equation 1
      delta <- K * (1 - Ew) # TODO: replace with K
      
      # update in elos table
      elos$elo[elos$item == eloresults$winner[i]] <- winner_elo + delta
      elos$elo[elos$item == eloresults$loser[i]] <- loser_elo - delta
    }
    
    # add id for iteration
    elos$it <- z
    
    # add to out
    out <- dplyr::bind_rows(out, elos)
  }
  
  # average elo across randomizations
  out <- out %>% 
    dplyr::group_by(item) %>% 
    dplyr::summarise(elo = mean(elo)) %>% 
    dplyr::filter(!item %in% c("1839loserdummyy!^", "1839winner!^dummyy"))
  
  return(out)
}
