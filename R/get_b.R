#' Get Analytical Estimate for Utility Coefficient from Raw Count Vectors
#'
#' This is a simple wrapper for get_b0, and it will compute utility
#' coefficients, standard errors, and confidence intervals for multiple
#' items in a best-worst scaling design.
#'
#' @param data A data.frame where each row represents an item, and three columns
#' represent total times shown to participants, total times selected as bests,
#' and total time selected as worsts
#' @param totals A string that is the name of the column for totals in the data
#' @param bests A string that is the name of the column for bests in the data
#' @param worsts A string that is the name of the column for worsts in the data
#' @param z A z-value to calculate the confidence intervals. Defaults to 1.96,
#' a 95\% CI
#' 
#' @return A data frame containing the utility coefficients, standard errors,
#' and upper and lower confidence intervals for each item (row) in the data
get_b <- function(data, totals, bests, worsts, z = 1.96) {
  data <- data[, c(totals, bests, worsts)]
  out <- apply(data, 1, function(x) {
    get_b0(x[[totals]], x[[bests]], x[[worsts]])
  })
  out <- as.data.frame(t(out))
  out$lb <- out$b - out$se * z
  out$ub <- out$b + out$se * z
  
  return(out)
}
