#' Correlation tests
#'
#' A wrapper for \code{\link[stats]{cor.test}} that returns htest data in a
#' data.frame rather than a list
#'
#' @param var1 a data.frame column to compare with \code{var2}
#' @param var2 a data.frame column to compare with \code{var1}
#' @param round logical, whether or not to round the results
#'
#' @return Returns a data.frame version of the standard htest output.
#' @export
#'
#' @importFrom stats cor.test
#'
#' @examples
#' cor_test(rnorm(1:100, 100), rnorm(1:100, 100), method = "pearson")
#'
cor_test = function(var1, var2, round = TRUE, ...){

  data1 = cor.test(var1, var2, ...)
  data1 = data.frame(t(unlist(data1)))

  grepList = c("statistic", "parameter", "p.value", "estimate", "conf.int")
  index = unlist(lapply(grepList, function(y) grep(y, names(data1))))
  for(i in index) {
    data1[, i] = as.numeric(as.character(data1[, i]))
  }

  if(round){
    for(i in index) {
      data1[, i] = round(data1[, i], 4)
    }
  }

  return(data1)
}
