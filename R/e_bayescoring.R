#' Empirical Bayes Method to Calculate Individual Best-Worst Scores
#'
#' @description
#' 
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
#' @param E ...
#' @param alpha ...
#' @param wide Logical of whether or not one wants the data returned in long
#'   (each row is an item-respondent combination and all best-worst scores are
#'   in the same column) format (FALSE) or in wide format (where each row is a 
#'   respondent, and the best-wost scores for the items are in their own 
#'   columns). See the `indiv` data as an example.
#' 
#' @return
#' 
#' 
#' @examples
#' 
#' 
#' @references 
#' 
#' 
#' @import magrittr
#' @import rlang
#' @export
e_bayescoring <- function(data, id, item, choice, E = .01, 
                          alpha = .1, wide = FALSE) {
  
  # get aggregate estimates ----
  agg_dat <- data %>% 
    dplyr::group_by(!!sym(item)) %>% 
    dplyr::summarise(
      bests = sum(!!sym(choice) == 1), 
      worsts = sum(!!sym(choice) == -1), 
      all = n()
    ) %>% 
    dplyr::mutate(p_j = (all - worsts + bests) / (2 * all)) %>% 
    dplyr::select(!!sym(item), p_j)
  
  # get naive individual estimates ----
  ind_dat <- data %>%
    dplyr::group_by(!!sym(id), !!sym(item)) %>% 
    dplyr::summarise(
      bests = sum(!!sym(choice) == 1), 
      worsts = sum(!!sym(choice) == -1), 
      all = n()
    ) %>% 
    dplyr::mutate(p_ij = (all - worsts + bests) / (2 * all)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(p_ij = dplyr::case_when(
      p_ij == 0 ~ E,
      p_ij == 1 ~ (1 - E),
      TRUE ~ p_ij
    ))
  
  # combine to empirical bayes ----
  out <- ind_dat %>% 
    dplyr::left_join(agg_dat, by = item) %>% 
    dplyr::mutate(
      p_ij = ((1 / (1 + alpha)) * p_ij) + ((alpha / (1 + alpha)) * p_j)
    ) %>% 
    dplyr::mutate(b_ebayes = log(p_ij / (1 - p_ij))) %>% 
    dplyr::select(!!sym(id), !!sym(item), b_ebayes)
  
  return(out)
}
