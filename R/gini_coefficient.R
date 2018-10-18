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


  participants <- unique(payments$from)

  # PARTICIPANT LEVEL ---------------------------------------------------------

  # Number of payments
  m <- payments %>% group_by(date, from) %>%
    summarise(num_payments = n())

  # Average liquidity provided
  liquidity_provided <-
    lapply(participants, function(x)
      max_liq_prov(x, payments, T))

  liquidity_provided <-
    do.call("rbind", liquidity_provided) %>%
    rename(from = participant)

  # Payments sent
  payments_sent <-
    payments %>%
    group_by(date, from) %>%
    summarise(total_value = sum(value, na.rm = T) / 1.0e+09)

  # Average Liquidity cost
  avg_liquidity_cost <-
    left_join(liquidity_provided, payments_sent, by = c("date", "from")) %>%
    mutate(cost = max_net_pos / total_value)

  #----------------------------------------------------------------------------

  # System Level --------------------------------------------------------------

  # Total number of payments made by all participants
  M <- payments %>% group_by(date) %>%
    summarise(num_payments = n())

  # MU - Average liquidity cost of system (volume weighted average
  # of participants payments)
  mu <- avg_liquidity_cost %>%
    group_by(date) %>%
    summarise(sys_cost = weighted_mean(cost, total_value))

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

  agg_particips <- left_join(avg_liquidity_cost, m, by = c("date", "from")) %>%
    group_by(date) %>%
    nest() %>%
    mutate(sums_particip = unlist(map(data, two_sums))) %>%
    select(-data)

  gini_df <- left_join(M, mu, by = "date") %>%
    left_join(agg_particips, by = "date") %>%
    mutate(gini = (1 / (2 * num_payments ^ 2 * sys_cost)) * sums_particip) %>%
    select(date, gini)

  return(gini_df)

}
