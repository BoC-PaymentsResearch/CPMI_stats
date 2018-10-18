#' Liquidity Provision Relative to Usage
#'
#' @param participant character value, Identifying code of the payments system
#'                    participant
#' @param payments dataframe value, payments data
#' @return dataframe structure, liquidity provision of the given participant
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
liq_prov_rel_usage <- function(participant, payments) {


  participants <- unique(payments$from)


  # Participant Level Liquidity Provided and Payments Sent --------------------
  participant_liq_provided <-
    max_liq_prov(participant, payments, T)

  participant_payments_sent <-
    payments %>%
    filter(from == participant) %>%
    group_by(date) %>%
    summarise(par_total_payments = sum(value))
  #----------------------------------------------------------------------------

  # Aggregate Level Liquidity Provided and Payments Sent ----------------------

  total_liquidity_provided <-
    lapply(participants,
           function(x)
             max_liq_prov(x, payments, debit = T))

  total_liquidity_provided <-
    do.call("rbind", total_liquidity_provided)

  total_liquidity_provided <- total_liquidity_provided %>%
    group_by(date) %>%
    summarise(sys_total_liquidity = sum(max_net_pos))

  total_payments_sent <- payments %>%
    group_by(date, from) %>%
    summarise(total = sum(value)) %>%
    group_by(date) %>%
    summarise(sys_total_payments = sum(total))
  #----------------------------------------------------------------------------

  liq_prov_rel_usage <- participant_liq_provided %>%
    left_join(total_liquidity_provided, by = "date") %>%
    left_join(participant_payments_sent, by = "date") %>%
    left_join(total_payments_sent, by = "date") %>%
    transmute(date = date, liq_prov = (max_net_pos / sys_total_liquidity) -
             (par_total_payments / sys_total_payments))


  return(liq_prov_rel_usage)

}
