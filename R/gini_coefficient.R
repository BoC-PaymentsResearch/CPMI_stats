#' Gini Coefficient
#'
#' @param payments dataframe value, payments data
#' @return dataframe structure, daily gini coefficient of the system
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
gini_coefficient <- function(payments) {


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

  # PARTICIPANT LEVEL ---------------------------------------------------------

  # Number of payments

  m <- payments[, .(num_payments = .N), by = .(date, from)]


  # Average liquidity provided
  liquidity_provided <-
    mclapply(participants, function(x)
      max_liq_prov(x, payments, T))

  liquidity_provided <-
    rbindlist(liquidity_provided)

  setnames(liquidity_provided, "participant", "from")

  # Payments sent
  payments_sent <-
    payments[, .(total_value = sum(value, na.rm = T) / 1.0e+09),
             by = .(date, from)]

  # Average Liquidity cost

  setkey(liquidity_provided, date, from)
  setkey(payments_sent, date, from)

  avg_liquidity_cost <- merge(liquidity_provided, payments_sent, all.x = TRUE)

  avg_liquidity_cost[, cost := max_net_pos / total_value]

  #----------------------------------------------------------------------------

  # System Level --------------------------------------------------------------

  # Total number of payments made by all participants
  M <- payments[, .(num_payments = .N), by = .(date)]

  # MU - Average liquidity cost of system (volume weighted average
  # of participants payments)

  mu <-
    avg_liquidity_cost[, .(sys_cost = weighted_mean(cost, total_value)), by = .(date)]

  #----------------------------------------------------------------------------

  # The Participant level data needs to be first aggregated to the daily level
  # so it can be merged with system level data

  # Calculating the sums in the gini coefficient
  two_sums <- function(df) {

    df <- na.omit(df)

    liq_sums <- sapply(df$from,
                       function(x)
                         sum(df$num_payments[which(df$from == x)] *
                               df$num_payments[which(df$from != x)] *
                               abs(df$cost[which(df$from == x)] - df$cost[which(df$from != x)])))

    return(sum(liq_sums))

  }

  setkey(avg_liquidity_cost, date, from)
  setkey(m, date, from)

  agg_particips <- merge(avg_liquidity_cost, m, all.x = TRUE)


  agg_particips <- agg_particips[, .(sums_particip = two_sums(.SD)), by = .(date)]


  setkey(M, date)
  setkey(mu, date)
  setkey(agg_particips, date)

  gini_df <- merge(M, mu, all.x = TRUE)

  gini_df <- merge(gini_df, agg_particips, all.x = TRUE)

  gini_df[, gini := (1 / (2 * num_payments ^ 2 * sys_cost)) * sums_particip]

  gini_df[, c("num_payments", "sys_cost", "sums_particip") := NULL]

  return(gini_df)

}
