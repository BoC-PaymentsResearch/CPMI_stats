time_to_seconds_from_midnight <- function(times) {


  seconds <- as.numeric(substring(times, 1, 2)) * 60 ^ 2 +
             as.numeric(substring(times, 4, 5)) * 60 +
             as.numeric(substring(times, 7, 8))

  return(seconds)
}
