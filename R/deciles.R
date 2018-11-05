#' Decile
#'
#' @param payments dataframe value, payments data
#' @return dataframe structure, daily time values of the deciles of payments
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
#' For each day in payments finds the time when the (0.1, 0.2, 0.3,..., 0.9)
#' percentiles of payments were made
#'
decile <- function(payments) {

  if(!"data.table" %in% class(payments)) {
    setDT(payments)
  }

  deciles <- seq(from = 0.1, to = 0.9, by = 0.1)

  total_payments_sent <-
    payments[, .(sys_total_payments = sum(value)), keyby = list(date)]


  first_decile <- function(X) {

    decile_times <-
      sapply(deciles,
             function(x) head(which(X >= x), 1))

    return(decile_times)
  }

  cum_sum_payments_sent <- copy(payments)

  cum_sum_payments_sent[order(date, time)]

  setorder(cum_sum_payments_sent, date, time)

  cum_sum_payments_sent[, cum_sum_payments := cumsum(value), by = list(date)]

  setkey(total_payments_sent, date)
  setkey(cum_sum_payments_sent, date)

  cum_sum_payments_sent <-
    merge(cum_sum_payments_sent, total_payments_sent, all.x = TRUE)

  cum_sum_payments_sent[, percent := cum_sum_payments / sys_total_payments, by = list(date)]

  cum_sum_payments_sent <-
    cum_sum_payments_sent[, .SD[first_decile(percent), .(time = time,
                                                         percent = percent,
                                                         percentile = deciles * 100)], by = list(date)]


  cum_sum_payments_sent[, percent := NULL]

  setorder(cum_sum_payments_sent, date, percentile, time)

  return(cum_sum_payments_sent)

}
