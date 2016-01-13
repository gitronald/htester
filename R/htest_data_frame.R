htest_data_frame <- function(htest.list) {
  # Converts list of htest lists into a data.frame
  # Automatically collects variable names
  # REPLACES ks.testm and cor.testm
  #
  data1 <- htest.list
  item.lengths <- sapply(data1, length) #Find max list length
  col.names <- names(data1[[which.max(item.lengths)]]) #Take col names from largest sub list

  if(sum(grepl("conf.int", col.names)) > 0) {
    for (i in 1:length(data1)) {
      data1[[i]][["ci.min"]] <- data1[[i]][["conf.int"]][1]
      data1[[i]][["ci.max"]] <- data1[[i]][["conf.int"]][2]
      data1[[i]][["conf.int"]] <- NULL
    }
    col.names <- col.names[!col.names == "conf.int"]
    col.names <- c(col.names, "ci.min", "ci.max")
  }

  # Create data receptacle
  stats.data <- setNames(data.frame(matrix(ncol = length(col.names),
                                           nrow = length(data1))),
                         nm = col.names)

  # Fill data receptacle
  for(i in col.names) {
    stats.data[, i] <- sapply(seq_along(data1), function(x) data1[[x]][[i]])
  }

  return(stats.data)
}
