#' Calculate Individual Best-Worst Scores
#'
#' @description 
#' Caclulate best-worst scores for each respondent-item combination. This is
#'   simply taking the total number of times a respondent selected the item
#'   as "best" and subtracting from that the number of times a respondent
#'   selected the item as "worst" (Louviere et al., 2013).
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
#' @param item A string of the name of the item column.
#' @param choice A string of the name of the choice column.
#' @param wide Logical of whether or not one wants the data returned in long
#'   (each row is an item-respondent combination and all best-worst scores are
#'   in the same column) format (FALSE) or in wide format (where each row is a 
#'   respondent, and the best-wost scores for the items are in their own 
#'   columns). See the `indiv` data as an example.
#' 
#' @return
#' A data.frame containing the id and item columns as well as a "bws" column
#'   that indicates the best worst score. If `wide = FALSE`, then each item
#'   has its own column and the bws is filled-in those columns.
#' 
#' @examples
#' data(indiv)
#' head(indiv)
#' indiv_bws(indiv, "id", "label", "value")
#' indiv_bws(indiv, "id", "label", "value", TRUE)
#' 
#' @references 
#' 
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @export
indiv_bws <- function(data, id, item, choice, wide = FALSE) {
  
  if (!all(data[[choice]] %in% -1:1))
    stop("The value column must consist of only -1s, 0s, and 1s")
  
  out <- data %>%
    dplyr::group_by(!!rlang::sym(id), !!rlang::sym(item)) %>%
    dplyr::summarise(
      bws = sum(!!rlang::sym(choice) == 1) - sum(!!rlang::sym(choice) == -1)
    ) %>%
    dplyr::ungroup()
  
  if (wide) {
    out <- tidyr::spread(out, !!rlang::sym(item), bws)
  }
  
  return(out)
}
