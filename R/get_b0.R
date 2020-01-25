#' Get Analytical Estimate for Utility Coefficient from Raw Counts
#'
#' This function uses equations 7, 10, 12, and 13 from
#' Lipovetsky & Conklin (2014) to calculate utility coefficient for an item
#' based off of the analytical (i.e., closed-form) estimation of the
#' multinomial logit model.
#'
#' @param total The total number of times an item was showed to all
#' participants. That is, it is how many blocks an item appeared in times
#' how many participants saw those blocks
#' @param best The total number of times an item was selected as "best"
#' @param worst The number of times an item was selected as "worst"
#' 
#' @references Lipovetsky, S., & Conklin, M. (2014). Best-worst scaling in
#' analytical closed-form solution. The Journal of Choice Modelling, 10,
#' 60-68. doi: 10.1016/j.jocm.2014.02.001
#' 
#' @return A named numeric vector of length 2, containing the utility
#' coefficient (b) and its associated standard error (se)
#' @noRd
get_b0 <- function(total, best, worst) {
  
  if (total < 0 | best < 0 | worst < 0) {
    stop("A total, best, or worst number cannot be less than zero.")
  }
  
  if (total < best + worst) {
    stop("Total cannot be less than the sum of best and worst.")
  }
  
  n_j <- total
  n_jw <- worst
  n_jb <- best
  
  p_j <- (n_j - n_jw + n_jb) / (2 * n_j)          # equation 7
  b_j <- log(p_j / (1 - p_j))                     # equation 10
  
  deltap_j <- sqrt((p_j * (1 - p_j)) / (2 * n_j)) # equation 12
  deltab_j <- deltap_j / (p_j * (1 - p_j))        # equation 13
  
  return(c(b = b_j, se = deltab_j))
}
