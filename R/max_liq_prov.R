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

  # correct column name check
  if(!all(colnames(payments) %in% c("ID", "date", "time", "value", "from", "to"))) {
    stop("The column names are incorrect. Please ensure the columns are named:
         ID, date, time, value, from, to")
  }

  # correct time formatting
  if(!"hms" %in% class(payments$time)) {
    stop("The payments column isn't in the correct format. It needs to be of class
         hms, use the function as.hms() to convert it")
  }

  if(!"data.table" %in% class(payments)) {
    setDT(payments)
  }

  payments <- payments[from == participant | to == participant]

  if(debit) {
    payments[from != participant, value := -value]
  } else {
    payments[from == participant, value := -value]
  }

  net_payments <- payments[, .(net = sum(value)), keyby = .(date, time)]

  net_payments <- net_payments[, .(net = cumsum(net)), keyby = .(date)]

  net_payments <-
    net_payments[, .(max_net_pos = max(net, 0) / 1.0e+09), by = .(date)]

  net_payments[, participant := participant]

  setcolorder(net_payments, c("date", "participant", "max_net_pos"))

  return(net_payments)
}
