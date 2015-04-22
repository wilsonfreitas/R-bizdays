
# [R-bizdays](http://aboutwilson.net/R-bizdays)

[![Build Status](https://travis-ci.org/wilsonfreitas/R-bizdays.svg?branch=master)](https://travis-ci.org/wilsonfreitas/R-bizdays)

`bizdays` deals with business days calculations based on a list of holidays (or nonworking days).
In many countries it is fairly usual the use of business days to price financial instruments.
Usually Excel's `NETWORKDAYS` is fairly at hand and once you have a list of holidays it is quite easy to put your data into a spreadsheet and make things happen.

R's users have similar feature in packages like `RQuantLib` and `timeDate`, but these solutions implement specific algorithms for each market (or country) and that is static, the user can't change that.
`bizdays` brings the flexbility for you to set the holidays and weekdays you want as nonworking days and has many functions to do calculations with that.


