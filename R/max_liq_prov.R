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

  if(!"data.table" %in% class(payments)) {
    setDT(payments)
  }

  payments_out <-
    payments[from == participant,
              .(payments_out = sum(value)),
              keyby = .(date, time)]

  payments_in <-
    payments[to == participant,
              .(payments_in = sum(value)),
              keyby = .(date, time)]

  setkey(payments_out, date, time)
  setkey(payments_in, date, time)

  net_payments <- merge(payments_out, payments_in, all = TRUE)

  # an NA value at t means no payment was sent at t
  net_payments[is.na(net_payments)] <- 0


  if(debit) {
    net_payments[, net := payments_out - payments_in]
  }
  else {
    net_payments[, net := payments_in - payments_out]
  }

  net_payments <-
    net_payments[, .(net =  cumsum(net)), by = .(date)]

  net_payments <-
    net_payments[, .(max_net_pos = max(net, 0) / 1.0e+09), by = .(date)]

  net_payments[, participant := participant]

  setcolorder(net_payments, c("date", "participant", "max_net_pos"))

  return(net_payments)
}
