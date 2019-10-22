#'
#' @import magrittr
#' @export
eloscoring <- function(data, id, block, item, choice, K = 30, iter = 100) {
  
  # do for all ids ----
  out <- lapply(unique(data[[id]]), function(cid) {
    get_eloresults(data[data[[id]] == cid, ], "block", "issue", "value") %>% 
      get_eloscores(K, iter) %>% 
      dplyr::mutate(id = cid)
  }) %>% 
    do.call(dplyr::bind_rows, .)
  
  # tidy results ----
  out <- out %>% 
    dplyr::select(id, item, elo)
  
  colnames(out) <- c(id, item, "eloscore")
  
  return(out)
}
