# https://github.com/STAT545-UBC/Discussion/issues/451
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("b_ebayes", "bests", "bibds", "bws", 
      "elo", "item", "p_ij", "p_j", "worsts")
  )
}
