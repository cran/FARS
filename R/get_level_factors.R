#' @title Extract Factors at a Given Hierarchical Level
#'
#' @description Retrieves all factors from a given level of the hierarchical structure that include the specified combination of blocks.
#'
#' @param factor_list A named list of factors for each node (from \code{build_factor_structure}).
#' @param combination Integer vector. Indices of the blocks involved in the current node.
#' @param level Integer. The hierarchical level (i.e., the number of blocks involved).
#'
#' @return A matrix of concatenated factors matching the given level and combination, or \code{NULL} if no match is found.
#'
#' @keywords internal
get_level_factors <- function(factor_list, combination, level) {

  matched <- list()

  for (key in names(factor_list)) {
    key_elements <- as.integer(strsplit(key, "-")[[1]])

    # Check if this key corresponds to the requested level and includes all in combination
    if (length(key_elements) == level && all(combination %in% key_elements)) {
      #matched[[length(matched) + 1]] <- factor_list[[key]]
      matched <- append(matched, list(factor_list[[key]]))
    }
  }

  # Combine matched factors if any found
  if (length(matched) > 0) {
    return(do.call(cbind, matched))
  } else {
    return(NULL)
  }
}
