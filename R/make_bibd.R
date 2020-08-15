#' Make Balanced Incomplete Block Designs from bibds Designs
#'
#' @description
#' This function generates a balanced incomplete block design. It takes one
#'   argument, the design number from the bibds data.frame object. See bibds.
#'
#' @param design Integer from 1 to 32. Corresponds to the characteristics from
#'   the bibds data.frame object.
#' @param seed Integer to set seed for reproducibility, such that the same
#'   design will be returned on different occasions. Defaults to 1839, so that
#'   the function will, by default, yield reproducible designs.
#'
#' @return A tibble. The first column indicates the block, and the rest of the
#'   columns indicate which item is in each block.
#'
#' @export
make_bibd <- function(design, seed = 1839) {
  
  if (!design %in% 1:32) stop("Enter an integer from 1 to 32")
  
  utils::data("bibds", package = "bwsTools", envir = environment())
  tmp <- bibds[bibds$design == design, ]
  set.seed(seed); out <- crossdes::find.BIB(tmp$t, tmp$b, tmp$k)
  colnames(out) <- paste0("Option", 1:ncol(out))
  rownames(out) <- 1:nrow(out)
  
  return(dplyr::as_tibble(out, rownames = "Block"))
}
