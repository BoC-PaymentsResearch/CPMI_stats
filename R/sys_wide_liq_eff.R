#' System Wide Liquidity Efficiency
#'
#' @param payments character value, specific payments data frame
#' @return dataframe structure, System wide liquidity efficiency for each day in
#'         inputed payments
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
sys_wide_liq_eff <- function(payments) {

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

  # Participant Level Liquidity Provided --------------------------------------
  total_liquidity_provided <-
    mclapply(participants,
           function(x)
             max_liq_prov(x, payments, debit = T))

  total_liquidity_provided <-
    rbindlist(total_liquidity_provided)

  total_liquidity_provided <-
    total_liquidity_provided[, .(total = sum(max_net_pos)), by = .(date)]

  #----------------------------------------------------------------------------

  # Aggregate Level Payments Made ---------------------------------------------

  total_payments_made <-
    payments[, .(total = sum(value)), by = .(date, from)]

  total_payments_made <-
    total_payments_made[, .(total = sum(total) / 1.0e+09), by = .(date)]


  total_payments_made[, efficiency := total / total_liquidity_provided$total]

  total_payments_made[, total := NULL]

  #----------------------------------------------------------------------------

  return(total_payments_made)

}
