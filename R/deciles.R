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


  deciles <- seq(from = 0.1, to = 0.9, by = 0.1)

  total_payments_sent <- payments %>%
    group_by(date) %>%
    summarise(sys_total_payments = sum(value))

  first_decile <- function(X) {
    decile_times <-
      sapply(deciles,
             function(x) head(which(X >= x), 1))

    return_vec <- vector(mode = "logical", length(X))
    return_vec[decile_times] <- TRUE

    return(return_vec)
  }

  cum_sum_payments_sent <- payments %>%
    arrange(date, time) %>%
    group_by(date) %>%
    mutate(cum_sum_payments = cumsum(value)) %>%
    left_join(total_payments_sent, by = "date") %>%
    mutate(percent = cum_sum_payments / sys_total_payments) %>%
    filter(first_decile(percent)) %>%
    select(date, time, percent) %>%
    mutate(percentile = (round(percent / 0.1) * 0.1) * 100) %>%
    select(date, percentile, time)


  return(cum_sum_payments_sent)

}


