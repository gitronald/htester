# htest_data_frame tests

a <- list()
a[[1]] <- cor.test(rnorm(10), rnorm(10))
a[[2]] <- cor.test(rnorm(10), rnorm(10))
a[[3]] <- cor.test(rnorm(10), rnorm(10))

b <- list()
b[[1]] <- ks.test(rnorm(10), rnorm(10))
b[[2]] <- ks.test(rnorm(10), rnorm(10))
b[[3]] <- ks.test(rnorm(10), rnorm(10))

c <- list()
c[[1]] <- wilcox.test(rnorm(10), rnorm(10))
c[[2]] <- wilcox.test(rnorm(10), rnorm(10))
c[[3]] <- wilcox.test(rnorm(10), rnorm(10))

d <- list()
d[[1]] <- t.test(rnorm(10), rnorm(10))
d[[2]] <- t.test(rnorm(10), rnorm(10))
d[[3]] <- t.test(rnorm(10), rnorm(10))

htest_data_frame(a) # cor.test
htest_data_frame(b) # ks.test
htest_data_frame(c) # wilcox.test
htest_data_frame(d) # t.test
