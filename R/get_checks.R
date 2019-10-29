#' Perform Checks That Data Follows Best Worst Scaling Design
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
get_checks <- function(data, id, block, item, choice) {
  
  # columns ----
  if (!all(c(id, block, item, choice) %in% names(data))) {
    stop("Data must contain ", id, ", ", block, ", ", item, ", ", choice)
  }
  
  # only -1, 0, and 1 ----
  if (!all(data[[choice]] %in% -1:1)) {
    stop("'", choice, "' column must only contain -1, 0, or 1")
  }
  
  # -1 and +1 only appear once per block per id ----
  test <- table(data[[choice]], data[[block]], data[[id]])
  test <- sapply(seq_len(dim(test)[3]), function(x) {
    all(apply(test[c(1, 3), , x], 1, function(y) y == 1))
  })
  if (!all(test)) {
    stop("-1 and 1 must appear exactly once in every id-block combination. ",
         "Currently, this is not the case for the ids:\n",
         paste0(unique(data[[id]])[!test], collapse = ", "))
  }
  
  # every block needs same amount of options ----
  if (length(unique(table(data[[id]], data[[block]]))) > 1) {
    stop("Each block for each id must have same amount of items")
  }
  
  # every id needs same amount of blocks ----
  if (length(unique(with(unique(data[, c(id, block)]), table(id)))) > 1) {
    stop("Each id must have the same amount of blocks")
  }
  
  # each item can't appear more than once in an id-block ----
  if (any(table(data[[id]], data[[block]], data[[item]]) > 1)) {
    stop("An item cannot appear more than once in an id-block combination.")
  }
  
  # each id must have the same items ----
  test <- lapply(unique(data[[id]]), function(x) {
    sort(unique(data[[item]][data[[id]] == x]))
  })
  if (!all(Vectorize(identical, "x")(test, test[[1]]))) {
    stop("Each id must be rating the same set of items")
  }
  
  # every pairwise item comparison must appear ----
  test <- sapply(unique(data[[id]]), function(x) {
    tmp <- crossprod(
      table(
        data[[block]][data[[id]] == x], 
        data[[item]][data[[id]] == x]
      )
    )
    any(tmp == 0)
  })
  if (any(test)) {
    stop("Each pairwise comparison between items must occur for every id")
  }
}
