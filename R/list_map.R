length_map <- function(data1) {
  length.map <- list_lengths(data1)
  length.map$lengths <- data.frame(t(sapply(length.map$lengths, c)))
  length.map$lengths <- setNames(cbind(paste0("test", 1:nrow(length.map$lengths)), length.map$lengths),
                                 c("test", col.names))
  return(length.map)
}
