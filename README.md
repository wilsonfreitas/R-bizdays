[cran-bizdays]: http://cran.r-project.org/web/packages/bizdays/index.html
[ANBIMA]: http://portal.anbima.com.br/Pages/home.aspx

# bizdays

[![Build Status](https://travis-ci.org/wilsonfreitas/R-bizdays.svg?branch=master)](https://travis-ci.org/wilsonfreitas/R-bizdays)

`bizdays` makes your life less miserable when you have to deal with business days calculations.
For many countries it is pretty much usual the use of business days to price financial instruments.
Usually Excel's `NETWORKDAYS` is fairly at hand and once you have a list of holidays this is quite easy to put your data into a spreadsheet and make things happen.

Although R's users have similar feature in packages like `RQuantLib` and `timeDate` it doesn't come for free.
Users have to do some stackoverflow and google in order to get this task accomplished.
`bizdays` tries to fit your needs. It is a tiny package which is dramactically focused on that simple task: calculations involving business days for a given list of holidays.
It doesn't implement specific holidays guessing algorithms, because I know that someway or another you will face a situation where that smart algorithm fails and that implementation will become useless.


## Installing

It is available on [CRAN][cran-bizdays] to be installed through:

```R
install.packages('bizdays')
```

or using `devtools`

```R
devtools::install_github('R-bizdays', username='wilsonfreitas')
```

## Using

You start by defining a `Calendar` object.

```R
library(bizdays)
cal <- Calendar(holidays, weekdays=c('sunday', 'saturday'))
```

where `holidays` is a sequence of dates which represents nonworking dates and the second argument, `weekdays`, is a sequence with nonworking weekdays.
`holidays` might be a sequence of strings with ISO formatted dates, `Date` (or `POSIX*`) objects or integers which represent dates in R.
`weekdays` must be a sequence of weekdays in words (lowercase).

Once you have instantiated a `Calendar` object you simply call `bizdays` function to get the amount of business days between 2 dates (or set of dates).

```{r}
business_days <- bizdays(from_dates, to_dates, cal)
```

> #### Why define weekdays?
> 
> I am frequently asked *Why do I have to define weekdays?* or even *Shouldn't it be `weekenddays` instead?*.
> 
> The reason I created `weekdays`:
> I want to provide a way to compute business days accordingly to any definition or satisfying any needs.
> In my world, the financial industry, weekends are nonworking days, but for those who work with events, for example, mondays migth be nonworking days.
> 
> `weekdays` defaults to `NULL` because I wanted the `Calendar()` call returned an [Actual](http://en.wikipedia.org/wiki/Day_count_convention#Actual_methods) calendar.
> So calling `Calendar(dib=365, name='Actual/365')` returns an [Actual/365](http://en.wikipedia.org/wiki/Day_count_convention#Actual.2F365_Fixed) calendar.


### The way I use `bizdays`

Since I am always using `holidaysANBIMA` holidays I create and set the default calendar in my `.Rprofile`

```{r}
library(bizdays)
cal <- Calendar(holidays=holidaysANBIMA, name='ANBIMA', weekdays=c('saturday', 'sunday'), dib=252)
bizdays.options$set(default.calendar=cal)
```

If you put these 3 lines of code at the end of your `.Rprofile` you don't have to instantiate the calendar every time you use `bizdays`.
You can simply

```{r}
business_days <- bizdays(from_dates, to_dates)
```

And it works with any `bizdays` functions.

## Holidays

I've included a dataset called `holidaysANBIMA` containing the list of holidays released by [ANBIMA][ANBIMA], this is quite useful at brazilian financial market.
So, if you have a specific list of holidays used at any market in the world, please share with me, I will be glad to include it in future releases.

