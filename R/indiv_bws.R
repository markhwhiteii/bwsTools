#' Calculate Individual Best-Worst Scores
#'
#' @description 
#'
#' @details 
#'
#' @param data
#' @param id
#' @param item
#' @param value
#' @param wide
#' 
#' @return 
#' 
#' @examples 
#' 
#' @references 
#' 
#' @importFrom magrittr `%>%`
#' @importFrom rlang `!!`
#' @export
ind_bws <- function(data, id, item, choice, wide = FALSE) {
  
  if (!all(data[[value]] %in% -1:1))
    stop("The value column must consist of only -1s, 0s, and 1s")
  
  out <- data %>%
    dplyr::group_by(!!rlang::sym(id), !!rlang::sym(item)) %>%
    dplyr::summarise(
      bws = sum(!!rlang::sym(choice) == 1) - sum(!!rlang::sym(choice) == -1)
    ) %>%
    dplyr::ungroup()
  
  if (wide) {
    out <- tidyr::spread(out, !!rlang::sym(item), bws)
  }
  
  return(out)
}
