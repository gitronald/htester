# Check spearman test ----------------------------------------------------

# Ronald Robertson
# Start: 1-14-2016

htest_data_frame(htests[["cor.test.spearman"]]) # Works with 1 warning
# Warning message:
#   In `[<-.data.frame`(`*tmp*`, , i, value = list(NULL, NULL, NULL)) :
#   provided 3 variables to replace 1 variables


# NOTES:
#  - Testing shows that the warning is due to the "parameter" return
#    value of cor.test coming up empty. Need to add if fix.

htest_data_frame2 <- function(htest.list) {
  # Converts list of htest lists into a data.frame
  # Automatically collects variable names
  # REPLACES ks.testm and cor.testm
  #
  data1 <- htests[["t.test.paired"]]
  item.lengths <- sapply(data1, length) # Find max list length
  col.names <- names(data1[[which.max(item.lengths)]]) # Take col names from largest sub list


    length.map <- list_lengths(data1)
    length.map <- data.frame(t(sapply(length.map$lengths, c)))
    length.map <- setNames(cbind(paste0("test", 1:nrow(length.map)), length.map),
                           c("test", col.names))


    if (max(sapply(length.map$overOne, length)) > 0) {
      sapply(seq_along(data1),
             function(x) sapply(seq_along(length.map$overOne),
                                function(y) names(data1[[x]][length.map$overOne[[y]]])), simplify = T)
    }


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



# Check htest_data_frame --------------------------------------------------

# Ronald Robertson
# Start: 1-14-2016

library(devtools)

load_all()
data("htests")

htest_data_frame(htests[["cor.test.pearson"]]) # Works with no warnings

htest_data_frame(htests[["cor.test.spearman"]]) # Works with 1 warning
# Warning message:
#   In `[<-.data.frame`(`*tmp*`, , i, value = list(NULL, NULL, NULL)) :
#   provided 3 variables to replace 1 variables

htest_data_frame(htests[["cor.test.kendall"]]) # Works with 1 warning
# Warning message:
#   In `[<-.data.frame`(`*tmp*`, , i, value = list(NULL, NULL, NULL)) :
#   provided 3 variables to replace 1 variables

htest_data_frame(htests[["ks.test"]]) # Works with no warnings

htest_data_frame(htests[["wilcox.test.paired"]]) # Works with 1 warning
# Warning message:
#   In `[<-.data.frame`(`*tmp*`, , i, value = list(NULL, NULL, NULL)) :
#   provided 3 variables to replace 1 variables

htest_data_frame(htests[["wilcox.test.unpaired"]]) # Works with 1 warning
# Warning message:
#   In `[<-.data.frame`(`*tmp*`, , i, value = list(NULL, NULL, NULL)) :
#   provided 3 variables to replace 1 variables

htest_data_frame(htests[["t.test.paired"]]) # Works with no warnings

htest_data_frame(htests[["t.test.unpaired"]]) # Fails with 1 error:
# Error in `[<-.data.frame`(`*tmp*`, , i, value = c(-0.113933238904906,  :
#                                                     replacement has 2 rows, data has 3

for (i in names(htests)) {
  x <- htest_data_frame(htests[[i]])
  print(x)
}

# Issue 1: t.test error ---------------------------------------------------

# Ronald Robertson
# Start: 1-12-2016

# Reproducible Example:

d <- list()
d[[1]] <- t.test(rnorm(10), rnorm(10))
d[[2]] <- t.test(rnorm(10), rnorm(10))
d[[3]] <- t.test(rnorm(10), rnorm(10))

htest_data_frame(d) # t.test

# Error Message:
#  Error in `[<-.data.frame`(`*tmp*`, , i, value = c(-0.301066775577742,  :
#  replacement has 2 rows, data has 3


item.lengths <- sapply(d, length)
col.names <- names(d[[which.max(item.lengths)]])

if(sum(grepl("conf.int", col.names)) > 0) {
  for (i in 1:length(d)) {
    d[[i]][["ci.min"]] <- d[[i]][["conf.int"]][1]
    d[[i]][["ci.max"]] <- d[[i]][["conf.int"]][2]
    d[[i]][["conf.int"]] <- NULL    # Remove column
  }
  col.names <- col.names[!col.names == "conf.int"]    # Remove name from name index
  col.names <- c(col.names, "ci.min", "ci.max")
}

# Create data structure
data_frame <- setNames(data.frame(matrix(ncol = length(col.names),
                                         nrow = length(d))),
                       nm = col.names)

# Fill data structure by row
for(i in col.names) {
  data_frame[, i] <- sapply(seq_along(d), function(x) d[[x]][[i]])
}


# New DEV -----------------------------------------------------------------


# Test lengths of each htest list item to detect conf.int or double estimate

list_lengths <- function(data1, overOne = TRUE) {
  data1 <- lapply(seq_along(data1),
                            function(x) {
                              sapply(names(data1[[x]]),
                                     function(y) length(data1[[x]][[y]]))
                  }
  )

  if (overOne) {
    max <- which(data1[[1]] > 1)
  } else {
    max <- NULL
  }

  new.data <- list(data <- data1,
                   max <- max)
  return (new.data)
}





# Test lengths of each htest list item to detect conf.int or double estimate
a.lengths <- lapply(seq_along(a), function(x) sapply(names(a[[x]]), function(y) length(a[[x]][[y]])))
b.lengths <- lapply(seq_along(b), function(x) sapply(names(b[[x]]), function(y) length(b[[x]][[y]])))

# Idenitify multiple values
names(which.max(a.lengths[[1]]))


sapply(a.lengths, c)

grepl(2, a.lengths)
grepl(2, b.lengths)

subset(a.lengths[[1]], a.lengths > 2)

a.lengths[a.lengths > 2, ]

if(sum(grepl("conf.int", col.names)) > 0) {
  for (i in 1:length(data1)) {
    data1[[i]][["ci.min"]] <- data1[[i]][["conf.int"]][1]
    data1[[i]][["ci.max"]] <- data1[[i]][["conf.int"]][2]
    data1[[i]][["conf.int"]] <- NULL    # Remove column
  }
  col.names <- col.names[!col.names == "conf.int"]    # Remove name from name index
  col.names <- c(col.names, "ci.min", "ci.max")
}


htest_data_frame <- function(htest.list) {
  # Converts list of htest lists into a data.frame
  # Automatically collects variable names
  # REPLACES ks.testm and cor.testm
  #
  data1 <- htest.list
  item.lengths <- sapply(data1, length) # Find max list length
  col.names <- names(data1[[which.max(item.lengths)]]) # Take col names from largest sub list

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
