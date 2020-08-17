#' Difference Method to Calculate Individual Best-Worst Scores
#'
#' @description 
#' Calculate best-worst scores for each respondent-item combination. This is
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
#' @param block A string of the name of the block column.
#' @param item A string of the name of the item column.
#' @param choice A string of the name of the choice column.
#' @param std Logical of whether or not one wants to standardize the data to
#'   a -1 to +1 range.
#' @param wide Logical of whether or not one wants the data returned in long
#'   (each row is an item-respondent combination and all best-worst scores are
#'   in the same column) format (FALSE) or in wide format (where each row is a 
#'   respondent, and the best-worst scores for the items are in their own 
#'   columns). See the `indiv` data as an example.
#' 
#' @return
#' A data.frame containing the id and item columns as well as a "bws" column
#'   that indicates the best worst score. If `wide = TRUE`, then each item
#'   has its own column and the bws is filled-in those columns.
#' 
#' @examples
#' data(indiv)
#' head(indiv)
#' diffscoring(indiv, "id", "block", "label", "value")
#' diffscoring(indiv, "id", "block", "label", "value", TRUE, TRUE)
#' 
#' @references 
#' Louviere, J., Lings, I., Islam, T., Gudergan, S., & Flynn (2013). An
#'   introduction to the application of (case 1) best-worst scaling in marketing
#'   research. International Journal of Research in Marketing, 30(3), 292-303.
#'   doi: 10.1016/j.ijresmar.2012.10.002
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang sym
#' @export
diffscoring <- function(data, id, block, item, choice, std = FALSE, 
                        wide = FALSE) {
  
  # check data ----
  get_checks(data, id, block, item, choice, nonbibd = TRUE)
  
  # get differences ----
  out <- data %>%
    dplyr::group_by(!!sym(id), !!sym(item)) %>%
    dplyr::summarise(
      bws = sum(!!sym(choice) == 1) - sum(!!sym(choice) == -1)
    ) %>%
    dplyr::ungroup()
  
  # standardize, if requested ----
  if (std) {
    key <- c(table(data[[item]]) / length(unique(data[[id]])))
    out$bws <- out$bws / key[out[[item]]]
  }
  
  # spread, if requested ----
  if (wide) {
    out <- tidyr::spread(out, !!sym(item), bws)
  }
  
  return(out)
}
