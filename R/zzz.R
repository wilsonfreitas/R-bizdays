
# Registering calendars

.onAttach <- function(libname, pkgname) {
  load_builtin_calendars()
}