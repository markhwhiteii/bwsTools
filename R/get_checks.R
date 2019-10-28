# TODO: add documentation
# TODO: need to add "id" to diffscoring and e_bayescoring for this to work
get_checks <- function(data, id, block, item, choice) {
  
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
  # TODO: takes far too long
  test <- sapply(unique(data[[id]]), function(x) {
    
    # make table of pairwise comparisons
    pcomps <- as.data.frame(
      t(combn(unique(data[[item]]), 2)), 
      stringsAsFactors = FALSE
    )
    pcomps$V3 <- FALSE
    
    # get data for just this id
    tmp <- data[data[[id]] == x, ]
    
    # for every block, check off the pairwise comparisons that occur
    for (y in unique(data[[block]])) {
      # current items
      ci <- tmp[tmp[[block]] == y, item, drop = TRUE]
      # mark as true if that pairwise comparison appears
      for (z in seq_len(nrow(pcomps))) {
        if ((pcomps[z, 1] %in% ci) & (pcomps[z, 2] %in% ci)) {
          pcomps[z, 3] <- TRUE
        }
      }
    }
    
    # return true if all appear
    all(pcomps$V3)
  })
  if (!all(test)) {
    stop("Each pairwise comparison between items must occur for every id")
  }
  
}
