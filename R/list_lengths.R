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
#' @examples
#' exlist1 <- list(list(1, 2, c(3, 0)),
#'                 list(1, 2, c(3, 0)),
#'                 list(1, 2, c(3, 0))
#'                 )
#' list_lengths(exlist1)
#'
#' exlist2 <- list(list(1, 2, 3),
#'                 list(1, 2, c(3, 0)),
#'                 list(1, 2, c(3, 0))
#' )
#' list_lengths(exlist2)
#'
#' exlist3 <- list(list(1, c(3, 0), c(3, 0)),
#'                 list(1, c(3, 0), c(3, 0)),
#'                 list(1, c(3, 0), c(3, 0))
#' )
#' list_lengths(exlist3)
list_lengths <- function(data1, overOne = TRUE) {
  data1 <- lapply(seq_along(data1),
                  function(x) {
                    sapply(seq_along(data1[[x]]),
                           function(y) length(data1[[x]][[y]]))
                  }
  )

  if (overOne) {
    max <- sapply(seq_along(data1), function(x) which(data1[[x]] > 1), simplify=F)
    list1 <- list(data1, max)
    names(list1) <- c("lengths", "overOne")
    return (list1)
  } else {
    return (data1)
  }
}


#' Create a map of lengths for items is a list
#'
#' @param data1 a list or list of lists composed of the same elements
#' @param data.frame logical, if TRUE return output as data.frame
#'
#' @return Returns a
#' @export
#'
#' @examples
list_map_lengths <- function(data1, data.frame = TRUE) {

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


# Level 1 List Map: Length
list_map_attributes <- function(data1, data.frame = TRUE) {
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
  return(map.lengths)
}


# Level 1 List Map: Length
list_map_names <- function(data1, data.frame = TRUE) {

  if(sum(grepl("statistic", names(lengths(df)))) > 0) {
        map.names <- names(lengths(df))
  } else {
    map.names <- lapply(seq_along(data1),  # For each element in data1
                        function(x) {      # Run this function
                          sapply(names(data1[[x]]),   # And for each element within each element in data1
                                 function(y) names(data1[[x]][y]) # Find the length
                          )
                        }
    )
  }


  if (data.frame == TRUE) {
    map.names <- setNames(as.data.frame(t(sapply(map.names, c))),
                          names(data1[[1]]))
  }
  return(map.names)
}





list_map <- function(data1, data.frame = T, type = c("names", "lengths")) {

#   if(length(which)>1) {
#     which <- 'sum'
#   }
#
#   if(!(which %in% c('sum','proportion'))){
#     stop("possible values for variable 'which' are c('sum','proportion').")
#   }

  map_type <- switch(type,
                     names = names,
                     lengths = length)

  map.names <- lapply(seq_along(data1),  # For each element in data1
                      function(x) {      # Run this function
                        sapply(names(data1[[x]]),   # And for each element within each element in data1
                               function(y) map_type(data1[[x]][[y]]) # Find the length
                        )
                      }
  )

  if (data.frame == TRUE) {
    map.names <- setNames(data.frame(t(sapply(map.names, c))),
                          names(data1[[1]]))
  }
  return(map.names)
}
