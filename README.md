# [R-bizdays](https://cran.r-project.org/package=bizdays) <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/bizdays)](https://cran.r-project.org/package=bizdays)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Downloads](http://cranlogs.r-pkg.org/badges/bizdays)](https://cran.r-project.org/package=bizdays)
[![R-CMD-check](https://github.com/wilsonfreitas/R-bizdays/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/wilsonfreitas/R-bizdays/actions/workflows/check-standard.yaml)
[![Coverage status](https://codecov.io/gh/wilsonfreitas/R-bizdays/branch/master/graph/badge.svg)](https://codecov.io/github/wilsonfreitas/R-bizdays?branch=master)
<!-- badges: end -->

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

## Calendars

`bizdays` comes with these calendars already loaded:

``` r
library(bizdays)
#> 
#> Attaching package: 'bizdays'
#> The following object is masked from 'package:stats':
#> 
#>     offset
calendars()
#> Calendars: 
#> actual, Brazil/ANBIMA, Brazil/B3, weekends
```

You can simply call bizdays declaring one of these.

``` r
following("2022-01-01", "Brazil/B3")
#> [1] "2022-01-03"
bizdays("2022-04-01", "2022-04-29", "Brazil/ANBIMA")
#> [1] 18
```
