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

  participants <- unique(payments$from)

  participant_liq_prov <- liq_prov_rel_usage(payments)

  agg_prov <-
    mclapply(participants, participant_liq_prov)

  agg_prov <-
    rbindlist(agg_prov)

  total_liquidity_provided <-
    agg_prov[liq_prov > 0, .(total = sum(liq_prov, na.rm = T)), keyby = .(date)]

  return(total_liquidity_provided)
}
