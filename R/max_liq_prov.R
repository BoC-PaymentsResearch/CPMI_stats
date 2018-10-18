#' Max Intraday Liquidity Provision
#'
#' @param participant character value, Identifying code of the payments system
#'                    participant
#' @param payments dataframe value, payments data
#' @param debit boolean value, logical value, if this value is TRUE then the net debit position
#'              is calculated. If FALSE then the net credit position is calculated.
#' @return dataframe structure, The max net debit/credit position of the given participant
#'         in billions
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
max_liq_prov <- function(participant, payments, debit) {


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

  # an NA value at t means no payment was sent at t
  net_payments[is.na(net_payments)] <- 0

  if (debit) {
    net_payments <-
      net_payments %>% mutate(net = payments_out - payments_in)

  } else {
    net_payments <-
      net_payments %>% mutate(net = payments_in - payments_out)
  }

  net_payments <- net_payments %>%
    mutate(net = cumsum(net)) %>%
    group_by(date) %>%
    summarise(max_net_pos = max(net, 0) / 1.0e+09) %>%
    mutate(participant = participant) %>%
    select(date, participant, max_net_pos)

  return(net_payments)
}
