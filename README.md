
# [R-bizdays]( https://cran.r-project.org/package=bizdays)

[![Build Status](https://travis-ci.org/wilsonfreitas/R-bizdays.svg?branch=master)](https://travis-ci.org/wilsonfreitas/R-bizdays)
[![Downloads](http://cranlogs.r-pkg.org/badges/bizdays?color=brightgreen)]( https://cran.r-project.org/package=bizdays)

[cran-bizdays]:  https://cran.r-project.org/package=bizdays
[ANBIMA]: http://portal.anbima.com.br/Pages/home.aspx

**bizdays** computes business days between dates based on collections of nonworking days and nonworking weekdays (usually weekends).
It also helps with other issues related to business days calculations like check whether a date is a business day, offset a date by a number of business days, adjust dates for the next or previous business day, create generators of business days sequences, and much more.
All functions are vectorizable so that speed up the calculations for large collections of dates.

## Installing

It is available on [CRAN][cran-bizdays] to be installed through:

```R
install.packages('bizdays')
```

or using `devtools`

```R
devtools::install_github('R-bizdays', username='wilsonfreitas')
```

## Holidays

I've included a dataset called `holidaysANBIMA` containing the list of holidays released by [ANBIMA][ANBIMA], this is quite useful at brazilian financial market.
So, if you have a specific list of holidays used at any market in the world, please share with me, I will be glad to include it in future releases.

