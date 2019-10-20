#' Walkscoring Method to Calculate Individual Best-Worst Scores
#'
#' @description 
#' Caclulate best-worst scores for each respondent-item combination. This uses
#'   the walkscoring method describe in CITE.
#'
#' @details
#' This function requires data to be in a specified format. Each row must
#'   represent a respondent-block-label combination. That is, it indicates
#'   the person, the block (or trial), the item that was judged, and a column
#'   indicating whether it was chosen as best (+1), worst (-1), or wasn't 
#'   selected as either (0).
#'
#' @param data A data.frame of the type described in details.
#' @param id A string of the name of the id column.
#' @param block A string of the name of the block column.
#' @param item A string of the name of the item column.
#' @param choice A string of the name of the choice column.
#' @param walks Integer indicating how many random walks to simulate.
#' 
#' @return
#' A data.frame containing the id and columns for each item, containing that
#'   individual's walkscore for that item.
#' 
#' @examples
#' data(indiv)
#' head(indiv)
#' walkscoring(indiv, "id", "block", "label", "value")
#' 
#' @references 
#' INSERT MY PAPER HERE LATER.
#' 
#' @export
walkscoring <- function(data, id, block, item, choice, walks = 10000) {
  
  # do for all ids ----
  out <- lapply(unique(data[[id]]), function(x) {
    B <- get_walkscores(
      get_M(data[data[[id]] == x, ], "b", block, item, choice),
      walks
    )
    W <- get_walkscores(
      get_M(data[data[[id]] == x, ], "w", block, item, choice),
      walks
    )
    rowMeans(cbind(scale(B), scale(W) * -1))
  })
  
  # tidy ----
  out <- do.call(rbind, out)
  out <- as.data.frame(cbind(id = unique(data[[id]]), out))
  colnames(out)[1] <- id
  
  return(out)
}
