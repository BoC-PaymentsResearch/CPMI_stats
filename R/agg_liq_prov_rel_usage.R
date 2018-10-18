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

  agg_prov <-
    lapply(participants,
           function(x)
             liq_prov_rel_usage(x, payments))

  agg_prov <-
    do.call("rbind", agg_prov)

  total_liquidity_provided <- agg_prov %>%
    filter(liq_prov > 0) %>%
    group_by(date) %>%
    summarise(total = sum(liq_prov, na.rm = T))

  return(total_liquidity_provided)
}
