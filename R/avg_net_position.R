#' Average Net Position
#'
#' @param participant character value, Identifying code of the payments system
#'                    participant
#' @param payments dataframe value, payments data
#' @param debit logical value, if this value is TRUE then the net debit position
#'              is calculated. If FALSE then the net credit position is calculated.
#' @param t_start character value, First time in the day when payments are sent.
#'                Format is HH:MM:SS (ex. 03:09:23)
#'                Defaults to NULL, taking the time of the first payment in the
#'                data as it's value.
#' @param t_end character value, Last time in the day when payments are sent. Format
#'              is HH:MM:SS (ex. 03:09:23)
#'              Defaults to NULL, taking the time of the last payment in the
#'              data as it's value.
#'
#' @return dataframe structure, The average net debit/credit position of the given participant
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
avg_net_position <- function(participant, payments, debit,
                             t_start = NULL, t_end = NULL) {

  payments_out <- payments %>%
    filter(from == participant) %>%
    group_by(date, time) %>%
    summarise(payments_out = sum(value))

  payments_in <- payments %>%
    filter(to == participant) %>%
    group_by(date, time) %>%
    summarise(payments_in = sum(value))

  net_payments <- full_join(payments_out, payments_in,
                            by = c("date", "time")) %>%
    arrange(date, time)

  net_payments[is.na(net_payments)] <- 0

  if (debit) {
    net_payments <-
      net_payments %>% mutate(net = payments_out - payments_in)

  } else {
    net_payments <-
      net_payments %>% mutate(net = payments_in - payments_out)
  }

  net_payments <- net_payments %>%
    mutate(net = cumsum(net))


  # Converting payment times to the number of seconds from midnight
  net_payments <- net_payments %>%
    mutate(time2 = time_to_seconds_from_midnight(time))


  t0 <-
    ifelse(
      !is.null(t_start) ,
      as.numeric(substring(t_start, 1, 2)) * 60 ^ 2 +
        as.numeric(substring(t_start, 4, 5)) * 60 +
        as.numeric(substring(t_start, 7, 8)),
      min(net_payments$time2)
    )

  T_final <-
    ifelse(
      !is.null(t_end),
      as.numeric(substring(t_end, 1, 2)) * 60 ^ 2 +
        as.numeric(substring(t_end, 4, 5)) * 60 +
        as.numeric(substring(t_end, 7, 8)),
      max(net_payments$time2)
    )

  net_payments <- net_payments %>%
    group_by(date) %>%
    mutate(
      dt = time2 - dplyr::lag(time2),
      dt = ifelse(row_number() == 1, first(time2) - t0, dt),
      Wt = (weighting(time2, T_final, t0)),
      dWt = (
        weighting(time2, T_final, t0) +
          weighting(dplyr::lag(time2), T_final, t0)
      ) / 2,
      dWt = ifelse(row_number() == 1, (1 + Wt) / 2, dWt),
      wn = dplyr::lag(net),
      wn = ifelse(row_number() == 1, 0, wn)
    ) %>%
    summarise(avg_net_pos = (2 / (T_final - t0)) *
                sum(pmax(wn, 0, na.rm = T) * dt * dWt, na.rm = T)) %>%
    mutate(avg_net_pos = avg_net_pos / 1.0e+09)

  return(net_payments)
}
