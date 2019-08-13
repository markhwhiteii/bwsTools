#' Get Analytical Choice Probability, Given a Vector of Utility Coefficients
#'
#' This takes a vector of utility coefficients and returns the same length
#' vector of analytical probabilities, using equation 18 from
#' Lipovetsky & Conklin (2014).
#'
#' @param b A numeric vector of utility coefficients
#' 
#' @references Lipovetsky, S., & Conklin, M. (2014). Best-worst scaling in
#' analytical closed-form solution. The Journal of Choice Modelling, 10,
#' 60-68. doi: 10.1016/j.jocm.2014.02.001
#' 
#' @return A vector of analytically-estimated utility coefficients
get_choice_p <- function(b) {
  exp(b) / sum(exp(b)) # equation 18
}
