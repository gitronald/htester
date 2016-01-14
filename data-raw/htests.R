# Data for testing htest_data_frame
# Naming convention: data. + name of test function + .method

htest.names <- c("cor.test.pearson",
                 "cor.test.spearman",
                 "cor.test.kendall",
                 "ks.test",
                 "wilcox.test.paired",
                 "wilcox.test.unpaired",
                 "t.test.paired",
                 "t.test.unpaired")

htest.length <- 3  # Set length of each list (i.e. how many times to conduct each test)
htests <- create_list(htest.names, htest.length)  # Create lists

## Fill lists

# Correlation Tests, Pearson's r
htests[["cor.test.pearson"]] <- lapply(seq(htest.length), function(x){
  htests[["cor.test.pearson"]][[x]] <- cor.test(rnorm(10), rnorm(10), method = "pearson")
})

# Correlation Tests, Spearman's rho
htests[["cor.test.spearman"]] <- lapply(seq(htest.length), function(x){
  htests[["cor.test.spearman"]][[x]] <- cor.test(rnorm(10), rnorm(10), method = "spearman")
})

# Correlation Tests, Kendall's tau
htests[["cor.test.kendall"]] <- lapply(seq(htest.length), function(x){
  htests[["cor.test.kendall"]][[x]] <- cor.test(rnorm(10), rnorm(10), method = "kendall")
})

# Kolmogorov-Smirnov Tests
htests[["ks.test"]] <- lapply(seq(htest.length), function(x){
  htests[["ks.test"]][[x]] <- ks.test(rnorm(10), rnorm(10))
})

# Wilcox Tests, Paired Samples (Wilcoxon signed rank test)
htests[["wilcox.test.paired"]] <- lapply(seq(htest.length), function(x){
  htests[["wilcox.test.paired"]][[x]] <- wilcox.test(rnorm(10), rnorm(10), paired = TRUE)
})

# Wilcox Tests, Unpaired Samples (Mann-Whitney / Wilcoxon rank sum test)
htests[["wilcox.test.unpaired"]] <- lapply(seq(htest.length), function(x){
  htests[["wilcox.test.unpaired"]][[x]] <- wilcox.test(rnorm(10), rnorm(10), paired = FALSE)
})

# t Tests, Paired Samples
htests[["t.test.paired"]] <- lapply(seq(htest.length), function(x){
  htests[["t.test.paired"]][[x]] <- t.test(rnorm(10), rnorm(10), paired = TRUE)
})

# t Tests, Unpaired Samples
htests[["t.test.unpaired"]] <- lapply(seq(htest.length), function(x){
  htests[["t.test.unpaired"]][[x]] <- t.test(rnorm(10), rnorm(10), paired = FALSE)
})

devtools::use_data(htests)
