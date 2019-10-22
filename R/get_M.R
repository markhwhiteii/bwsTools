#' Calculate Adjacency Matrix for Walkscoring Method
#'
#' @description 
#' Takes best-worst scoring data in tidy format and transforms the relations
#'   between items into an adjacency matrix to be graphed.
#'
#' @details
#' This function requires data to be in a specified format. Each row must
#'   represent a respondent-block-label combination. That is, it indicates
#'   the person, the block (or trial), the item that was judged, and a column
#'   indicating whether it was chosen as best (+1), worst (-1), or wasn't 
#'   selected as either (0).
#'
#' @param data A data.frame of the type described in details.
#' @param bw String of "b" or "w" indicating scoring of best or worst matrix.
#' @param block A string of the name of the block column.
#' @param item A string of the name of the item column.
#' @param choice A string of the name of the choice column.
#' @param normal Logical of whether or not one wants to normalize the data so 
#'   that each row sums to 1.
#' 
#' @return
#' A symmetric adjacency matrix representing the relationship between items.
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang sym
get_M <- function(data, bw, block, item, choice, normal = TRUE) {
  
  # prepare matrix ----
  ui <- length(unique(data[[item]])) # num. of unique items
  M <- matrix(NA, nrow = ui, ncol = ui)
  diag(M) <- 0
  colnames(M) <- rownames(M) <- unique(data[[item]])
  
  # score for best ----
  if (bw == "b") {
    for (r in 1:nrow(M)) {
      cn <- rownames(M)[r]
      for (b in 1:length(unique(data[[block]]))) {
        cb <- data[data[[block]] == b, c(item, choice)] %>% 
          dplyr::group_by(!!sym(item)) %>% 
          dplyr::summarise(score = sum(!!sym(choice)))
        for (i in cb[[item]]) {
          if (!cn %in% cb[[item]])
            NULL
          else if ((cb$score[cb[[item]] == i] - cb$score[cb[[item]] == cn]) < 1)
            M[r, i] <- 0
          else
            M[r, i] <- cb$score[cb[[item]] == i] - cb$score[cb[[item]] == cn]
        }
      }
    }
  }
  
  # score for worst ----
  if (bw == "w") {
    for (r in 1:nrow(M)) {
      cn <- rownames(M)[r]
      for (b in 1:length(unique(data[[block]]))) {
        cb <- data[data[[block]] == b, c(item, choice)] %>% 
          dplyr::group_by(!!sym(item)) %>% 
          dplyr::summarise(score = sum(!!sym(choice)))
        for (i in cb[[item]]) {
          if (!cn %in% cb[[item]])
            NULL
          else if ((cb$score[cb[[item]] == cn] - cb$score[cb[[item]] == i]) < 1)
            M[r, i] <- 0
          else
            M[r, i] <- cb$score[cb[[item]] == cn] - cb$score[cb[[item]] == i]
        }
      }
    }
  }
  
  # normalize ----
  if (normal) {
    for (r in 1:nrow(M)) {
      if (sum(M[r, ]) == 0)
        M[r, ] <- 1 / nrow(M)
      else
        M[r, ] <- M[r, ] / sum(M[r, ])
    }
  }
  
  return(M)
}
