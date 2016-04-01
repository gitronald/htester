#' Convert an htest list, or a list of htest lists to a data.frame
#'
#' @param htest.list a single htest list or a list of htest lists
#'
#' @return
#'    Returns a \code{data.frame} with data from one or more htests
#' @export
#'
#' @examples
#' data(htests)
#' htest_data_frame(htests[["cor.test.pearson"]])
#'
htest_data_frame <- function(htest.list) {

  data1 <- htest.list
  item.lengths <- sapply(data1, length)                # Find max list length
  col.names <- names(data1[[which.max(item.lengths)]]) # Take col names from largest sub list

  # Map list lengths
  ll <- list_map_lengths(data1)
  empty.cols <- names(which(sapply(ll, sum) == 0))      # Find all empty columns
  multi.cols <- names(which(apply(ll, 2, max) > 1))     # Find all columns containing > 1 values
  col.names <- col.names[!(col.names %in% empty.cols)]  # Remove empty cols from name index

  if(length(multi.cols > 0)) {
    if("conf.int" %in% multi.cols) {
      for (i in 1:length(data1)) {
        data1[[i]][["ci.min"]] <- data1[[i]][["conf.int"]][1]
        data1[[i]][["ci.max"]] <- data1[[i]][["conf.int"]][2]
        data1[[i]][["conf.int"]] <- NULL    # Remove column
      }
      col.names <- col.names[!col.names == "conf.int"]    # Remove name from name index
      col.names <- c(col.names, "ci.min", "ci.max")
    }
    if("estimate" %in% multi.cols) {
      for (i in 1:length(data1)) {
        data1[[i]][["estimate.x"]] <- data1[[i]][["estimate"]][1]
        data1[[i]][["estimate.y"]] <- data1[[i]][["estimate"]][2]
        data1[[i]][["estimate"]] <- NULL    # Remove column
      }
      col.names <- col.names[!col.names == "estimate"]    # Remove name from name index
      col.names <- c(col.names, "estimate.x", "estimate.y")
    }
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

#' Convert an htest list, or a list of htest lists to a data.frame
#'
#' @param htest.list a single htest list or a list of htest lists
#'
#' @return
#'    Returns a \code{data.frame} with data from one or more htests
#' @export
#'
#' @examples
#' data(htests)
#' htest_data_frame(htests[["cor.test.pearson"]])
#'
dfh <- function(htest.list) {

  data1 <- htest.list
  item.lengths <- sapply(data1, length)                # Find max list length
  col.names <- names(data1[[which.max(item.lengths)]]) # Take col names from largest sub list

  # Map list lengths
  ll <- list_map_lengths(data1)
  empty.cols <- names(which(sapply(ll, sum) == 0))      # Find all empty columns
  multi.cols <- names(which(apply(ll, 2, max) > 1))     # Find all columns containing > 1 values
  col.names <- col.names[!(col.names %in% empty.cols)]  # Remove empty cols from name index

  if(length(multi.cols > 0)) {
    if("conf.int" %in% multi.cols) {
      for (i in 1:length(data1)) {
        data1[[i]][["ci.min"]] <- data1[[i]][["conf.int"]][1]
        data1[[i]][["ci.max"]] <- data1[[i]][["conf.int"]][2]
        data1[[i]][["conf.int"]] <- NULL    # Remove column
      }
      col.names <- col.names[!col.names == "conf.int"]    # Remove name from name index
      col.names <- c(col.names, "ci.min", "ci.max")
    }
    if("estimate" %in% multi.cols) {
      for (i in 1:length(data1)) {
        data1[[i]][["estimate.x"]] <- data1[[i]][["estimate"]][1]
        data1[[i]][["estimate.y"]] <- data1[[i]][["estimate"]][2]
        data1[[i]][["estimate"]] <- NULL    # Remove column
      }
      col.names <- col.names[!col.names == "estimate"]    # Remove name from name index
      col.names <- c(col.names, "estimate.x", "estimate.y")
    }
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
