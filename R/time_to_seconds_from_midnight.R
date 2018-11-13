time_to_seconds_from_midnight <- function(times) {

  seconds <- as.numeric(difftime(times, as.hms(0), units = "secs"))

  return(seconds)
}
