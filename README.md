# htester

<!--
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dtables)](http://cran.r-project.org/package=dtables)
[![CRAN_Downloads_Badge](http://cranlogs.r-pkg.org/badges/grand-total/dtables)](http://cranlogs.r-pkg.org/badges/grand-total/dtables)
[![Build Status](https://travis-ci.org/gitronald/dtables.svg?branch=master)](https://travis-ci.org/gitronald/dtables)
-->

* The goal of htester is to convert the output of R's htests - `cor.test`, `wilcox.test`, `ks.test`, etc - into data.frame formats.
* If you have any questions or want to help out feel free to send me an email or a pull request! My email can be found in the DESCRIPTION.

### Getting Started
``` {r}
# Install Github version:
devtools::install_github("gitronald/htester")

# Load the package and sample data
library(htester)
data(htests)
```

### htests
* R's `htest` outputs are returned in a list format that renders well in the console, but aren't ideal for writing out in table form. For example, consider the output of `cor.test`:

``` {r}
> x = rnorm(100, 100, 40)
> y = rnorm(100, 100, 40)
> cor.test(x, y, method = "spearman")
```
```
    Spearman's rank correlation rho

data:  x and y
S = 136820, p-value = 0.0748
alternative hypothesis: true rho is not equal to 0
sample estimates:
      rho 
0.1790099 
```

* htester functions provide the same tests, but return results in data.frame:

``` {r}
> cor_test(x, y)
```
```
  statistic.S p.value estimate.rho null.value.rho alternative                          method     data.name
1      136818  0.0748        0.179              0   two.sided Spearman's rank correlation rho var1 and var2
```



### htests available

* Correlation tests: `cor_test`
* Komogorov-Smirnov: `ks_test`
* Wilcox tests: `wilcox_test`
* t-tests: `t_test`