#' Gini Coefficient
#'
#' @param payments dataframe value, payments data
#' @param liquidity_provided dataframe value, optional parameter of a dataframe
#'                           containing the maximum net debit positions of all
#'                           participants. If not provided then this function
#'                           calculates these positions
#' @return dataframe structure, daily gini coefficient of the system
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to.
#'
gini_coefficient <- function(payments, liquidity_provided) {

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

  participants <- unique(payments$from)

  # PARTICIPANT LEVEL ---------------------------------------------------------

  # Number of payments

  if(missing(liquidity_provided)) {

    liquidity_provided <-
      mclapply(participants, function(x)
        max_liq_prov(x, payments, T))

    liquidity_provided <-
      rbindlist(liquidity_provided)

  } else {
    liquidity_provided <- copy(liquidity_provided)
  }

  setnames(liquidity_provided, "participant", "from")

  payments_sent <-
    payments[, .(total_value = sum(value, na.rm = T),
                 num_payments = .N),
             by = .(date, from)]

  setkey(liquidity_provided, date, from)
  setkey(payments_sent, date, from)

  avg_liquidity_cost <- merge(liquidity_provided, payments_sent, all.x = TRUE)

  avg_liquidity_cost <- avg_liquidity_cost[num_payments != 0]

  avg_liquidity_cost[, cost := max_net_pos / total_value]

  # SYSTEM LEVEL --------------------------------------------------------------

  # MU - Average liquidity cost of system (volume weighted average
  # of participants payments)

  mu <-
    avg_liquidity_cost[, .(sys_cost = weighted_mean(cost, num_payments)), by = .(date)]

  #----------------------------------------------------------------------------

  # The Participant level data needs to be first aggregated to the daily level
  # so it can be merged with system level data

  two_sums <- function(liq_cost, num_pmts) {

    ts <- seq_along(liq_cost)
    n <- length(liq_cost)

    liq_sums <-
      (sapply(ts[-n], function(x)
        sum(
          abs(liq_cost[seq(x + 1, n)] - liq_cost[x]) * num_pmts[seq(x + 1, n)] * num_pmts[x]
        )))

    return(sum(liq_sums))

  }

  setorder(avg_liquidity_cost, date, cost)

  agg_particips <-
    avg_liquidity_cost[, .(sums_particip = two_sums(cost, num_payments),
                           total_payments = sum(.SD$num_payments)), by = .(date)]

  setkey(mu, date)
  setkey(agg_particips, date)

  gini_df <- merge(mu, agg_particips, all.x = TRUE)

  gini_df[, gini := (sums_particip / (total_payments ^ 2 * sys_cost))]

  gini_df[, c("total_payments", "sys_cost", "sums_particip") := NULL]

  return(gini_df)

  }
