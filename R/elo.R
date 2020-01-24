#' Elo Method to Calculate Aggregate Best-Worst Scores
#'
#' @description
#' Calculate aggregate best-worst scores using Elo scoring. This specific
#'   application comes from Hollis (2018). It makes each individual/block 
#'   pairwise comparisons and updates Elo scores based on who won and lost those
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
#' 
#' @return
#' A data.frame containing the item column as well as an "elo" column that 
#'   indicates the Elo score.
#' 
#' @examples
#' data(agg)
#' head(agg)
#' # run more than 1 iter; just doing 1 here for speed
#' elo(agg, "pid", "trial", "character", "decision", iter = 1)
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
elo <- function(data, id, block, item, choice, K = 30, iter = 100) {
  
  # perform checks, but fewer because aggregate ----
  get_checks(data, id, block, item, choice, TRUE)
  
  # get results ----
  tmp <- get_eloresults(data, block, item, choice, TRUE, id)
  
  # get scores from results ----
  out <- get_eloscores(tmp, K, iter)
  
  return(out)
}
