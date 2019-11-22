#' Elo Method to Calculate Individual Best-Worst Scores
#'
#' @description
#' Calculate individual best-worst scores using Elo scoring. This specific
#'   application comes from Hollis (2018). It makes each block pairwise
#'   comparisons and updates Elo scores based on who won and lost those
#'   comparisons. No ties are considered, which occurs between all of the items
#'   that have not been selected as either best or worst. Hollis (2018) also
#'   recommends adding two "dummy items": one that defeats every other item,
#'   and one that loses to every other item. This is employed here. The default 
#'   K is 30, per Hollis (2018). Since Elo is temporal in nature, Hollis also
#'   recommends running various iterations, each with a different randomization
#'   of the order of matchups. The default is the 100 used by Hollis. These are 
#'   averaged together to calculate individual Elo best-worst scores. Elo scores
#'   are all initialized at 1000.
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
#' @param K The Elo K-factor. The default is 30, per Hollis (2018).
#' @param iter Number of different randomizations of the "matchup" order to 
#'   iterate through. The default is Hollis's (2018, 2019) recommendation.
#' @param wide Logical of whether or not one wants the data returned in long
#'   (each row is an item-respondent combination and all best-worst scores are
#'   in the same column) format (FALSE) or in wide format (where each row is a 
#'   respondent, and the best-worst scores for the items are in their own 
#'   columns). See the `indiv` data as an example.
#' 
#' @return
#' A data.frame containing the id and item columns as well as a "elo" 
#'   column that indicates the Elo score. If `wide = TRUE`, then 
#'   each item has its own column and the Elo score is filled-in those columns.
#' 
#' @examples
#' data(indiv)
#' head(indiv)
#' # run more than 1 iter; just doing 1 here for speed
#' eloscoring(indiv, "id", "block", "label", "value", iter = 1)
#' 
#' @references 
#' Hollis, G. (2018). Scoring best-worst data in unbalanced many-item designs,
#'   with applications to crowdsourcing semantic judgments. Behavior Research
#'   Methods, 50(2), 711-729. doi: 10.3758/s13428-017-0898-2
#'
#' Hollis, G. (2019). The role of number of items per trial in best-worst
#'   scaling experiments. Behavior Research Methods. doi: 
#'   10.3758/s13428-019-01270-w
#' 
#' @importFrom magrittr "%>%"
#' @importFrom rlang sym
#' @export
eloscoring <- function(data, id, block, item, choice, K = 30, iter = 100,
                       wide = FALSE) {
  
  # check data ----
  get_checks(data, id, block, item, choice)
  
  # do for all ids ----
  out <- lapply(unique(data[[id]]), function(cid) {
    get_eloresults(data[data[[id]] == cid, ], block, item, choice) %>% 
      get_eloscores(K, iter) %>% 
      dplyr::mutate(id = cid)
  })
  out <- do.call(dplyr::bind_rows, out)
  
  # tidy results ----
  out <- out %>% 
    dplyr::select(id, item, elo)
  
  colnames(out) <- c(id, item, "elo")
  
  # pivot wide, if requested ----
  if (wide) {
    out <- out %>% 
      tidyr::spread(!!sym(item), elo)
  }
  
  return(out)
}
