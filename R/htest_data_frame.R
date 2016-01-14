#' Convert an htest list, or a list of htest lists to a data.frame
#'
#' @param htest.list a single htest list or a list of htest lists
#'
#' @return
#'    Returns a \code{data.frame} with data from one or more htests
#' @export
#'
#' @examples
#' a <- list()
#' a[[1]] <- cor.test(rnorm(10), rnorm(10))
#' a[[2]] <- cor.test(rnorm(10), rnorm(10))
#' a[[3]] <- cor.test(rnorm(10), rnorm(10))
#' htest_data_frame(a)
#'
htest_data_frame <- function(htest.list) {
  # Converts list of htest lists into a data.frame
  # Automatically collects variable names
  # REPLACES ks.testm and cor.testm
  #
  data1 <- htest.list
  item.lengths <- sapply(data1, length) # Find max list length
  col.names <- names(data1[[which.max(item.lengths)]]) # Take col names from largest sub list

#
#   ll <- list_lengths(data1)
#   if (max(sapply(ll$overOne, length)) > 1) {
#     sapply(seq_along(d), function(x) sapply(seq_along(ll$overOne), function(y) names(d[[x]][ll$overOne[[y]]])), simplify = F)
#   }


  if(sum(grepl("conf.int", col.names)) > 0) {
    for (i in 1:length(data1)) {
      data1[[i]][["ci.min"]] <- data1[[i]][["conf.int"]][1]
      data1[[i]][["ci.max"]] <- data1[[i]][["conf.int"]][2]
      data1[[i]][["conf.int"]] <- NULL    # Remove column
    }
    col.names <- col.names[!col.names == "conf.int"]    # Remove name from name index
    col.names <- c(col.names, "ci.min", "ci.max")
  }

  # Create data structure
  data_frame <- setNames(data.frame(matrix(ncol = length(col.names),
                                           nrow = length(data1))),
                         nm = col.names)

  # Fill data structure by row
  for(i in col.names) {
    data_frame[, i] <- sapply(seq_along(data1), function(x) data1[[x]][[i]])
  }

  return(data_frame)
}
