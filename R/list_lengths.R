#' Obtain list of list lengths
#'
#' Summarize the length of all items in a nested list with the the option of
#' identifying lengths over one.
#'
#' @param data1 a list of lists
#' @param overOne logical, \code{TRUE} returns index of values over one
#'
#' @return A list showing the lengths of the nested list items and the index for
#'   idenitfying items with a length greater than one.
#' @export
#'
#' @examples
#' exlist <- list(list(1, 2, c(3, 0)),
#'                list(1, c(3, 0), 2),
#'                list(1, c(3, 0), 2)
#'                )
#'
#' list_lengths(exlist)
#'
list_lengths <- function(data1, overOne = TRUE) {
  data1 <- lapply(seq_along(data1),
                  function(x) {
                    sapply(seq_along(data1[[x]]),
                           function(y) length(data1[[x]][[y]]))
                  }
  )

  if (overOne) {
    max <- sapply(seq_along(data1), function(x) which(data1[[x]] > 1), simplify=F)
    list1 <- list(data1,
                  max)
    names(list1) <- c("length", "overOne")
    return (list1)
  } else {
    return (data1)
  }
}
