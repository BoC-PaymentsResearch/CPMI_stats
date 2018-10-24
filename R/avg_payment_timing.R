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

  avg_payment_time <- payments %>%
    mutate(time_secs = time_to_seconds_from_midnight(time)) %>%
    group_by(date, time) %>%
    summarise(
      sys_total_payments = sum(value),
      sys_total_payments_t = sum(value * time_secs)
    ) %>%
    group_by(date) %>%
    summarise(avg_time = sum(sys_total_payments_t) / sum(sys_total_payments)) %>%
    mutate(
      hours = floor(avg_time / (60 ^ 2)),
      minutes = floor((avg_time / 60) %% 60),
      seconds = floor(avg_time %% 60),
      avg_time = paste0(
        ifelse(hours < 10, "0", ""),
        hours,
        ":",
        ifelse(minutes < 10, "0", ""),
        minutes,
        ":",
        ifelse(seconds < 10, "0", ""),
        seconds
      )
    ) %>%
    select(date, avg_time)

  return(avg_payment_time)
}


