# [R-bizdays](https://cran.r-project.org/package=bizdays)

<!-- [![R-CMD-check](https://github.com/wilsonfreitas/bizdays/workflows/R-CMD-check/badge.svg)](https://github.com/wilsonfreitas/bizdays/actions) -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bizdays)](https://cran.r-project.org/package=bizdays)
[![Downloads](http://cranlogs.r-pkg.org/badges/bizdays?color=brightgreen)](https://cran.r-project.org/package=bizdays)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/wilsonfreitas/R-bizdays?branch=master&svg=true)](https://ci.appveyor.com/project/wilsonfreitas/R-bizdays)
[![Coverage
status](https://codecov.io/gh/wilsonfreitas/R-bizdays/branch/master/graph/badge.svg)](https://codecov.io/github/wilsonfreitas/R-bizdays?branch=master)

**bizdays** computes business days between dates based on collections of
nonworking days and nonworking weekdays (usually weekends). It also
helps with other issues related to business days calculations like check
whether a date is a business day, offset a date by a number of business
days, adjust dates for the next or previous business day, create
generators of business days sequences, and much more. All functions are
vectorizable so that speed up the calculations for large collections of
dates.

## Installing

It is available on [CRAN](https://cran.r-project.org/package=bizdays) to
be installed through:

``` r
install.packages('bizdays')
```

or using `devtools`

``` r
devtools::install_github('R-bizdays', username='wilsonfreitas')
```

## Holidays

I've included a dataset called `holidaysANBIMA` containing the list of
holidays released by [ANBIMA](https://www.anbima.com.br/), this is quite
useful at brazilian financial market. So, if you have a specific list of
holidays used at any market in the world, please share with me, I will
be glad to include it in future releases.
