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


  participants <- unique(payments$from)

  # Participant Level Liquidity Provided --------------------------------------
  total_liquidity_provided <-
    lapply(participants,
           function(x)
             max_liq_prov(x, payments, debit = T))

  total_liquidity_provided <-
    do.call("rbind", total_liquidity_provided)

  total_liquidity_provided <- total_liquidity_provided %>%
    group_by(date) %>%
    summarise(total = sum(max_net_pos))
  #----------------------------------------------------------------------------


  # Aggregate Level Payments Made ---------------------------------------------
  total_payments_made <- payments %>%
    group_by(date, from) %>%
    summarise(total = sum(value)) %>%
    group_by(date) %>%
    summarise(total = sum(total) / 1.0e+09)

  system_wide_liquidity <-
    total_payments_made %>%
    mutate(efficiency = total / total_liquidity_provided$total) %>%
    select(-total)
  #----------------------------------------------------------------------------

  return(system_wide_liquidity)

}
