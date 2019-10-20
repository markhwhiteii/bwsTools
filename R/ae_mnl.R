#' Analytical Estimation of a Multinomial Logit Model for Best-Worst Scaling
#'
#' @description 
#' This uses Equations 7, 10, 12, 13, and 18 from Lipovetsky &
#'   Conklin (2014) to take vectors of total times shown to participants, total 
#'   times selected as best, and total times selected as worst. It uses their 
#'   closed-form solution to calculate utility coefficients—as well as their 
#'   standard errors and confidence intervals—and choice probabilities.
#'
#' @param data A data.frame where each row represents an item, and three columns
#'   represent total times shown to participants, total times selected as bests,
#'   and total time selected as worsts.
#' @param totals A string that is the name of the column for totals in the data.
#' @param bests A string that is the name of the column for bests in the data.
#' @param worsts A string that is the name of the column for worsts in the data.
#' @param z A z-value to calculate the confidence intervals. Defaults to 1.96,
#'   a 95\% CI.
#' 
#' @references 
#' Lipovetsky, S., & Conklin, M. (2014). Best-worst scaling in analytical 
#'   closed-form solution. The Journal of Choice Modelling, 10, 60-68. 
#'   doi: 10.1016/j.jocm.2014.02.001
#' 
#' @return A data.frame containing the utility coefficients (with standard error
#'   and confidence intervals) and choice probabilities for each item (row)
#'   in the data.
#' 
#' @examples
#' # Replicate Table 6 from Lipovetsky & Conklin (2014)
#' d <- data.frame(
#'   totals = c(7145, 7144, 7144, 7144, 7145, 7145, 7144, 7146, 8166, 
#'              7145, 7144, 7144, 7145, 7144, 7145, 7144, 7146),
#'   bests = c(1733, 968, 5218, 2704, 2307, 692, 1816, 689, 2483, 1422, 
#'             362, 2589, 4158, 825, 829, 859, 966),
#'   worsts = c(1324, 2139, 113, 1010, 772, 3986, 1438, 2397, 1041, 1538, 
#'   4597, 966, 305, 2875, 2256, 2259, 1604)
#' )
#' results <- ae_mnl(d, "totals", "bests", "worsts")
#' (d <- cbind(d, results))
#' 
#' @export
ae_mnl <- function(data, totals, bests, worsts, z = 1.96) {
  out <- get_b(data, totals, bests, worsts, z)
  out$choice_p <- get_choice_p(out$b)
  
  return(out)
}
