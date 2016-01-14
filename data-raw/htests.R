# Data for testing htest_data_frame
#
# Naming convention: data. + name of test function + .method


htest.names <- c("data.cor.test.pearson",
                 "data.ks.test",
                 "data.wilcox.test.wilcox",
                 "data.t.test")

htests <- sapply(htests.names, function(x) lapply(x, function(x) vector("list", length = 3)))


# Correlation Tests, Pearson
htests[["data.cor.test.pearson"]] <- lapply(seq(1:3), function(x){
  htests[["data.cor.test.pearson"]][[x]] <- cor.test(rnorm(10), rnorm(10), method = "pearson")
})

# Kolmogorov-Smirnov Tests
htests[["data.ks.test"]] <- lapply(seq(1:3), function(x){
  htests[["data.ks.test"]][[x]] <- ks.test(rnorm(10), rnorm(10), method = "pearson")
})

htests[["data.cor.test.pearson"]][[1]] <- cor.test(rnorm(10), rnorm(10), method = "pearson")

data.cor.test.pearson <- list()
data.pearson[[1]] <- cor.test(rnorm(10), rnorm(10), method = "pearson")
data.pearson[[2]] <- cor.test(rnorm(10), rnorm(10), method = "pearson")
data.pearson[[3]] <- cor.test(rnorm(10), rnorm(10), method = "pearson")



#
# b <- list()
# b[[1]] <- ks.test(rnorm(10), rnorm(10))
# b[[2]] <- ks.test(rnorm(10), rnorm(10))
# b[[3]] <- ks.test(rnorm(10), rnorm(10))
#
# c <- list()
# c[[1]] <- wilcox.test(rnorm(10), rnorm(10))
# c[[2]] <- wilcox.test(rnorm(10), rnorm(10))
# c[[3]] <- wilcox.test(rnorm(10), rnorm(10))
#
# d <- list()
# d[[1]] <- t.test(rnorm(10), rnorm(10))
# d[[2]] <- t.test(rnorm(10), rnorm(10))
# d[[3]] <- t.test(rnorm(10), rnorm(10))
#
# htest_data_frame(a) # cor.test
# htest_data_frame(b) # ks.test
# htest_data_frame(c) # wilcox.test
# htest_data_frame(d) # t.test
#
#
#
#
# exlist1 <- list(list(1, 2, c(3, 0)),
#                 list(1, 2, c(3, 0)),
#                 list(1, 2, c(3, 0))
#                 )
# list_lengths(exlist1)
#
# exlist2 <- list(list(1, 2, 3),
#                 list(1, 2, c(3, 0)),
#                 list(1, 2, c(3, 0))
# )
# list_lengths(exlist2)
#
# exlist3 <- list(list(1, c(3, 0), c(3, 0)),
#                 list(1, c(3, 0), c(3, 0)),
#                 list(1, c(3, 0), c(3, 0))
# )
# list_lengths(exlist3)
