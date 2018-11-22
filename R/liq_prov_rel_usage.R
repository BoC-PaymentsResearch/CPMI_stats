#' Liquidity Provision Relative to Usage
#'
#' @param participant character value, Identifying code of the payments system
#'                    participant
#' @param payments dataframe value, payments data
#' @param total_liquidity_provided dataframe value, daily system wide liquidity provided
#'                           defaults to NULL, the aggregate version of this
#'                           function calls the function with non NULL value
#' @param total_payments_sent dataframe value, daily payments sent
#'                            defaults to NULL, the aggregate version of this function
#'                            calls the function with non NULL value
#'
#' @return dataframe structure, liquidity provision of the given participant
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
liq_prov_rel_usage <- function(participant, payments,
                               total_liquidity_provided = NULL,
                               total_payments_sent = NULL) {


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

  participants <- unique(payments$from)


  # Participant Level Liquidity Provided and Payments Sent --------------------
  participant_liq_provided <-
    max_liq_prov(participant, payments, T)

  participant_payments_sent <-
    payments[from == participant,
             .(par_total_payments = sum(value, na.rm = T)), by = .(date)]

  #----------------------------------------------------------------------------

  # Aggregate Level Liquidity Provided and Payments Sent ----------------------


  if(is.null(total_liquidity_provided) && is.null(total_payments_sent)) {

  total_liquidity_provided <-
    mclapply(participants,
           function(x)
             max_liq_prov(x, payments, debit = T))

  total_liquidity_provided <-
    rbindlist(total_liquidity_provided)

  total_liquidity_provided <-
    total_liquidity_provided[, .(sys_total_liquidity = sum(max_net_pos)),
                             keyby = .(date)]

  total_payments_sent <- payments[, .(sys_total_payments = sum(value)),
                                  keyby = .(date)]
  }


  #----------------------------------------------------------------------------

  setkey(participant_liq_provided, date)
  setkey(total_liquidity_provided, date)
  setkey(participant_payments_sent, date)
  setkey(total_payments_sent, date)

  liq_prov_rel_usage <-
    merge(participant_liq_provided, total_liquidity_provided, all.x = TRUE)

  liq_prov_rel_usage <-
    merge(liq_prov_rel_usage, participant_payments_sent, all.x = TRUE)

  liq_prov_rel_usage <-
    merge(liq_prov_rel_usage, total_payments_sent, all.x = TRUE)

  liq_prov_rel_usage[is.na(liq_prov_rel_usage)] <- 0

  liq_prov_rel_usage[, liq_prov := (max_net_pos / sys_total_liquidity) -
                       (par_total_payments / sys_total_payments)]

  liq_prov_rel_usage[, c("participant", "max_net_pos", "sys_total_liquidity",
                         "par_total_payments", "sys_total_payments") := NULL]

  return(liq_prov_rel_usage)

}
