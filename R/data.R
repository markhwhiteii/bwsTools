#' Balanced Incomplete Block Designs for use in Best-Worst Scaling
#'
#' @description
#' A data.frame of 32 different balanced incomplete block designs (BIBD) that
#'   can be used to set up best-worst scaling studies. The columns are: design,
#'   which is an identification number that can is fed to get_bibd; t, which
#'   is the number of items (or "treatments") one will be using in the study;
#'   k, which is how many of the items the respondent sees per trial; r, which
#'   is how many times the participant sees each item across all of the trials;
#'   b, which is how many trials (or "blocks") there are in the design; and
#'   lambda, which indicates how many times each pair of options co-occur
#'   throughout the design. This is taken from Table 11.3 in Cochran & Cox 
#'   (1957), including only the designs where t and b are both less than or
#'   equal to 20 (as any larger would put cognitive strain on a respondent).
#'
#' @references
#' Cochran, W. G., & Cox, G. M. (1957). Balanced and partially balanced 
#'   incomplete block designs. In Experimental designs (2nd ed., pp. 439–482).
#'   New York: John Wiley & Sons, Inc.
"bibds"

#' Example Data for Individual-Level Best-Worst Scaling
#'
#' @description
#' A tibble that shows the format a tibble should take when it is
#'   submitted to a individual `*scoring` function.
"indiv"
