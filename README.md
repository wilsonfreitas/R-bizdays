[cran-bizdays]: http://cran.r-project.org/web/packages/bizdays/index.html
[ANBIMA]: http://portal.anbima.com.br/Pages/home.aspx

# bizdays

[![Build Status](https://travis-ci.org/wilsonfreitas/R-bizdays.svg?branch=master)](https://travis-ci.org/wilsonfreitas/R-bizdays)

`bizdays` makes your life less miserable when you have to deal with business days calculations.
For many countries it is pretty much usual the use of business days to price financial instruments.
Usually Excel's `NETWORKDAYS` is fairly at hand and once you have a list of holidays it is quite easy to put your data into a spreadsheet and make things happen.
Although R's users have similar feature in packages like `RQuantLib` and `timeDate` it doesn't come for free.
Users have to do some stackoverflow and google in order to get this task accomplished.
`bizdays` tries to fit your needs. It is a tiny package which is dramactically focused on this simple task: calculations involving business days for a given list of holidays.
It doesn't implement specific holidays guessing algorithms, because I know that someway or another you will face a situation where that smart algorithm fails and that implementation will become useless.
Many financial institutions have to handle with nasty contract's rules from many different countries and each market has its own rules for counting the business days.


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

