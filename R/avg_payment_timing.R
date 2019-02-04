#' Average Time of Payments Sent
#'
#' @param payments dataframe value, payments data
#'
#' @return dataframe structure, daily time values when the average payment
#'         was sent through the system
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
avg_payment_timing <- function(payments) {

  # correct column name check
  if(!all(colnames(payments) %in% c("ID", "date", "time", "value", "from", "to"))) {
    stop("The column names are incorrect. Please ensure the columns are named:
         ID, date, time, value, from, to")
  }

  # correct time formatting
  if(!"hms" %in% class(payments$time)) {
    stop("The time column isn't in the correct format. It needs to be of class
         hms, use the function as.hms() to convert it")
  }

  if(!"data.table" %in% class(payments)) {
    setDT(payments)
  }

  avg_payment_time <- copy(payments)

  avg_payment_time[, time_secs := time_to_seconds_from_midnight(time)]

  avg_payment_time <-
    avg_payment_time[, .(sys_total_payments = sum(value),
                         sys_total_payments_t = sum(value * time_secs)),
                     by = .(date, time)]

  avg_payment_time <-
    avg_payment_time[,
                     .(avg_time = sum(sys_total_payments_t) / sum(sys_total_payments)),
                     by = .(date)]

  avg_payment_time[, hours := floor(avg_time / (60 ^ 2))
                   ][, minutes := floor((avg_time / 60) %% 60)
                     ][, seconds := floor(avg_time %% 60)
                       ][,avg_time := paste0(
                         ifelse(hours < 10, "0", ""),
                         hours,
                         ":",
                         ifelse(minutes < 10, "0", ""),
                         minutes,
                         ":",
                         ifelse(seconds < 10, "0", ""),
                         seconds
                       )]

  avg_payment_time[, c("hours", "minutes", "seconds") := NULL]

  return(avg_payment_time)
}
