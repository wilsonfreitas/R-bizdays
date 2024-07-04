# bizdays 1.0.16

  * BUG in reverse index (issue #108)

# bizdays 1.0.15

  * `Brazil/BMF` Calendar updated: addition of November 20th holiday from 2024 ahead.
  
  * `Brazil/ANBIMA` Calendar updated: addition of November 20th holiday from 2024 ahead.
  
  * `Brazil/B3` Calendar updated: added holidays from 2024.

# bizdays 1.0.14

  * `Brazil/BMF` Calendar updated: corrected holidays in the period between 1990-2000

  * `Brazil/B3` Calendar updated: added holidays in the period between 1990-2000

  * added more tests to `getbizdays`

# bizdays 1.0.13

  * `Brazil/B3` Calendar updated: included 2023 holidays

# bizdays 1.0.12

  * added new calendar `Brazil/BMF` which extends `Brazil/ANBIMA` starting at 1990-01-01 and is based on the trading days observed in future contracts.

# bizdays 1.0.11

  * improved getdate to use a day as reference and allows expressions like:
    `getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")`,
    `getdate("next wed", Sys.Date())`, ...(issue #28)

  * organized `ref` code to avoid duplicate code

# bizdays 1.0.10

  * holidaysB3 data updated, the day 2020-07-09 has been removed, it's not a holiday.

  * load_builtin_calendars function created to load calendars: actual, Brazil/ANBIMA, Brazil/B3, weekends

  * Depends updated to 4.0

  * removed data/holidaysANBIMA.rda and data/holidaysB3.rda, calendars are loaded from JSON files

# bizdays 1.0.9

  * New function getbizdays which returns the number of business days for specific periods of a year or a month (issue #94)

  * Use .onAttach to load build in calendars (issue #88)

  * New B3 calendar with holidays up to the of 2022 (issue #93)
  
  * load_quantlib_calendars and load_rmetrics_calendars have a financial argument to define if the loaded calendar is a financial calendar
  
  * [BUG] wrong return for negative bizdays in non financial calendars (issue #92)

# bizdays 1.0.8

  * Corrected typos in documentation (links formatting) in order to fulfill CRAN requirements

  * Updated travis CI Yaml to avoid errors due to missing requirements

# bizdays 1.0.7

  * Disabled removed the use of ref object as getdate argument

# bizdays 1.0.6

  * Import and export calendars (issue #25)

  * Implemented getdate function (issue #28)
  
  * Renamed functions (replaced dots with _) (issue #76)
  
# bizdays 1.0.5

  * Implemented bizdays optimisations (issue #70)

  * New vignette "Financial and non financial calendars"

  * Improved Calendar's print method, now it is more informative (issue #68)

  * Introduced the financial argument to create.calendar. It allows to create non financial calendars (issue #62)
  
  * Removed the old calendars construction: the Calendar function

# bizdays 1.0.4

  * Implemented bizdiff function (issue #57).
  
  * Defined start and end dates for Rmetrics calendars (issue #60)
  
  * Implemented new Calendar methods: holidays and weekdays (issue #61)

# bizdays 1.0.3

  * Implemented check in tests for suggested packages: RQuantLib and timeDate (issue #56).

# bizdays 1.0.2

  * Implemented the double index strategy to avoid inconsistencies in business days counting (issue #54)
  
  * Added has.calendars function to check if a calendar exists
  
  * Implemented requireNamespace check for Suggedted packages: RQuantLib and timeDate
  
  * offset function has been vectorized
  
  * bug fixes (issues #53, #54, #55)
  
# bizdays 1.0.1

  * Minor changes to achieve conformity with CRAN policies

# bizdays 1.0.0

  * Calendar's dib argument, bizyears and bizyearse were removed

  * Load calendars from RQuantLib and timeDate (Rmetrics) packages
  
  * create.calendar function, Calendar have been deprecated and will be removed (not exported) soon.
  
  * Updated LICENSE file
  
  * bizdays accepts from > to arguments returning negative values in such cases
  
  * new following and preceding functions equal adjust.next and adjust.previous
  
  * new modified.following and modified.preceding functions
  
  * new calendar register: calendars must be created with create.calendar and are referenced by its name in bizdays methods.

# bizdays 0.2.2

  * Calendar accepts POSIX* in holidays
  
  * Calendar's start.date and end.date are set to default values only 
  when their aren't provided
  
  * Docs updated

# bizdays 0.2.1

  * changed print.Calendar to be more informative
  
  * new offset function create (add.bizdays alias)
  
  * offset (add.bizdays) accepts vector of numbers (n argument)
  
  * updated documentation
  
  * added vignettes

  * renamed default.calendar to Calendar(name='Actual/365', dib=365)

# bizdays 0.2.0

  * print.Calendar returns invisible(x) and shows weekdays
  
  * Calendar raises a warning when holidays is set and weekdays is not
  
  * Calendar's dib and name defaults to NULL
  
  * default.calendar is Calendar(name='Actual', dib=365)
  
  * bizyears raises an error if dib is NULL
  
  * add.bizdays performance improved

# bizdays 0.1.5

  * add function has been renamed to add.bizdays

  * Calendar's argument weekdays default value is NULL

  * Calendar has new arguments: dib, adjust.from, adjust.to

  * travis-ci integration

  * New functions: bizyears, bizyearse, bizdayse
  
  * bizdays accepts NA values in both arguments, from and to

# bizdays 0.1.4

  * bizdays, adjust.previous, adjust.next and is.bizday accept POSIXct
  and POSIXlt objects.
  
  * bizdays, adjust.previous, adjust.next and is.bizday handle NA values
  without break

  * bizdays, adjust.next and adjust.previous are fast
