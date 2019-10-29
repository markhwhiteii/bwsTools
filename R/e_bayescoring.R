#' Empirical Bayes Method to Calculate Individual Best-Worst Scores
#'
#' @description
#' Individual utilities from empirical bayes estimations. Instead of doing the
#'   computationally-expensive hierarchical Bayesian multinomial logistic
#'   regression model, Lipovetsky & Conklin (2015) show an empirical Bayes
#'   way to calculate this analytically. This function calculates choice
#'   probabilities shown using Equation 10 in Lipovetsky & Conklin (2015) and
#'   transforms them to be on a linear regression coefficient scale. Default
#'   values for the E and alpha parameters are those performing best in their
#'   simulations.
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
#' @param E Value of precision shown in Equation 8 of Lipovetsky & Conklin
#'   (2015). If the naive estimate for a choice probability is 0, it is replaced
#'   with E; If the naive estimate for the choice probability is 1, i is
#'   replaced with 1 - E.
#' @param alpha The mixing parameter shown in Equation 10 of Lipovetsky &
#'   Conklin (2015). This shapes how much the naive individual estimate and how
#'   much of the aggregate estimate influences the resulting estimate.
#' @param wide Logical of whether or not one wants the data returned in long
#'   (each row is an item-respondent combination and all best-worst scores are
#'   in the same column) format (FALSE) or in wide format (where each row is a 
#'   respondent, and the best-wost scores for the items are in their own 
#'   columns). See the `indiv` data as an example.
#' 
#' @return
#' A data.frame containing the id and item columns as well as a "b_ebayes" 
#'   column that indicates the utility coefficient. If `wide = TRUE`, then 
#'   each item has its own column and the coefficient is filled-in 
#'   those columns.
#' 
#' @examples
#' data(indiv)
#' head(indiv)
#' e_bayescoring(indiv, "id", "block", "label", "value")
#' 
#' @references 
#' Lipovetsky, S., & Conklin, M. (2015). MaxDiff priority estimations with and
#'   without HB-MNL. Advances in Adaptive Data Analysis, 7(1). doi:
#'   10.1142/S1793536915500028
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang sym
#' @export
e_bayescoring <- function(data, id, block, item, choice, E = .01,
                          alpha = .1, wide = FALSE) {
  
  # check data ----
  get_checks(data, id, block, item, choice)
  
  # get aggregate estimates ----
  agg_dat <- data %>% 
    dplyr::group_by(!!sym(item)) %>% 
    dplyr::summarise(
      bests = sum(!!sym(choice) == 1), 
      worsts = sum(!!sym(choice) == -1), 
      all = dplyr::n()
    ) %>% 
    dplyr::mutate(p_j = (all - worsts + bests) / (2 * all)) %>% # equation 4
    dplyr::select(!!sym(item), p_j)
  
  # get naive individual estimates ----
  ind_dat <- data %>%
    dplyr::group_by(!!sym(id), !!sym(item)) %>% 
    dplyr::summarise(
      bests = sum(!!sym(choice) == 1), 
      worsts = sum(!!sym(choice) == -1), 
      all = dplyr::n()
    ) %>% 
    dplyr::mutate(p_ij = (all - worsts + bests) / (2 * all)) %>% # equation 4
    dplyr::ungroup() %>% 
    # equation 8:
    dplyr::mutate(p_ij = dplyr::case_when(
      p_ij == 0 ~ E,
      p_ij == 1 ~ (1 - E),
      TRUE ~ p_ij
    ))
  
  # combine to get empirical bayes ----
  out <- ind_dat %>% 
    dplyr::left_join(agg_dat, by = item) %>% 
    dplyr::mutate(
      # equation 10:
      p_ij = ((1 / (1 + alpha)) * p_ij) + ((alpha / (1 + alpha)) * p_j)
    ) %>% 
    dplyr::mutate(b_ebayes = log(p_ij / (1 - p_ij))) %>% # equation 5
    dplyr::select(!!sym(id), !!sym(item), b_ebayes)
  
  # pivot wide, if requested ----
  if (wide) {
    out <- out %>% 
      tidyr::spread(!!sym(item), b_ebayes)
  }
  
  return(out)
}
