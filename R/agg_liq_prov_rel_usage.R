#' Aggregate Liquidity Provision Relative to Usage
#'
#' @param payments dataframe value, payments data
#' @return dataframe structure, system wide measure for liquidity provision relative
#'         to usage
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
#' Calculates the daily aggregate (across participants) liquidity provision. The
#' aggregation is calculated as the sum of the positive liquidity provision relative
#' to usage values.
#'
agg_liq_prov_rel_usage <- function(payments) {

  participants <- unique(payments$from)

  total_liquidity_provided <-
    mclapply(participants,
           function(x)
             max_liq_prov(x, payments, debit = T))

  total_liquidity_provided <-
    do.call("rbind", total_liquidity_provided)

  total_liquidity_provided <-
    total_liquidity_provided[, .(sys_total_liquidity = sum(max_net_pos)),
                             keyby = .(date)]

  total_payments_sent <- payments[, .(sys_total_payments = sum(value)),
                                  keyby = .(date)]


  agg_prov <-
    mclapply(participants,
           function(x)
             liq_prov_rel_usage(x, payments,
                                total_liquidity_provided = total_liquidity_provided,
                                total_payments_sent = total_payments_sent))

  agg_prov <-
    do.call("rbind", agg_prov)

  total_liquidity_provided <-
    agg_prov[liq_prov > 0, .(total = sum(liq_prov, na.rm = T)), keyby = .(date)]

  return(total_liquidity_provided)
}
