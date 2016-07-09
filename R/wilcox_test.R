#' Wilcoxon Rank Sum and Signed Rank Tests
#'
#' A wrapper for \code{\link[stats]{wilcox.test}} that returns htest data in a
#' data.frame rather than a list
#'
#' @param var1 a vector to correlate with var2
#' @param var2 a vector to correlate with var1
#' @param method the correlation method to use, either pearson, spearman, or
#'   kendall
#' @param round logical, whether or not to round the results
#'
#' @return Returns a data.frame version of the standard htest output. See
#'   \code{ks.test} for additional arguments and further detail
#' @export
#'
#' @importFrom stats wilcox.test
#'
#' @examples
#' wilcox_test(rnorm(10, 5), rnorm(10, 5))
#'
wilcox_test = function(var1, var2, round = TRUE, ...){

  data1 = wilcox.test(var1, var2, ...)
  data1 = data.frame(t(unlist(data1)))

  grepList = c("statistic", "parameter", "p.value", "estimate", "conf.int")
  index = unlist(lapply(grepList, function(y) grep(y, names(data1))))
  for(i in index) {
    data1[, i] = as.numeric(as.character(data1[, i]))
  }

  if(round){
    for(i in index) {
      data1[, i] = format(round(data1[, i], 4), nsmall = 4)
    }
  }

  return(data1)
}
