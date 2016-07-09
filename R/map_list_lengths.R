#' Generate a data.frame map of a list's lengths, or the lengths of lists within a list
#'
#' @param data1 a list or list of lists composed of the same elements
#' @param data.frame logical, if TRUE return output as data.frame
#'
#' @return Returns a table or data.frame with the lengths of each list element
#' @export
#'
#' @examples
#' map_list_lengths(htests[[1]])
#' map_list_lengths(htests[[5]])
#' map_list_lengths(htests[[5]][[1]])
#'
map_list_lengths <- function(data1, data.frame = TRUE) {

  if(sum(grepl("statistic", names(lengths(data1)))) > 0) {  # Check if single list
    map.lengths <- lengths(data1)

    if (data.frame == TRUE) {
      map.lengths <- setNames(data.frame(t(sapply(map.lengths, c))),
                              names(data1))
    }

  } else {
    map.lengths <- lapply(seq_along(data1),  # For each element in data1
                          function(x) {      # Run this function
                            sapply(seq_along(data1[[x]]),   # And for each element within each element in data1
                                   function(y) length(data1[[x]][[y]]) # Find the length
                            )
                          }
    )
    if (data.frame == TRUE) {
      map.lengths <- setNames(data.frame(t(sapply(map.lengths, c))),
                              names(data1[[1]]))
    }
  }

  return(map.lengths)
}
