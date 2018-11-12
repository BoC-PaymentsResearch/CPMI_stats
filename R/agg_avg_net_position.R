#' Aggregate Average Net Position
#'
#' @param payments dataframe value, payments data
#' @param debit logical value, if this value is TRUE then the net debit position
#'              is calculated. If FALSE then the net credit position is calculated.
#'
#' @param central_bank character value, If the central bank is to be removed from
#'                     calculation then this parameter should be the identifier
#'                     of the central bank. Defaults to NULL meaning no participant
#'                     is removed from the calculation
#' @param t_start time value, First time in the day when payments are sent. Should
#'                have the format: HH:MM:SS.
#'                Defaults to NULL, taking the time of the first payment in the
#'                data as it's value.
#' @param t_end time value, Last time in the day when payments are sent.Should
#'              have the format: HH:MM:SS.
#'              Defaults to NULL, taking the time of the last payment in the
#'              data as it's value.
#' @return dataframe structure, The aggregate net debit/credit
#'         position of the system
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
#' Calculates the daily aggregate (across participants), average net debit/credit position.
#' The average net debit/credit positions of each participant is calculated with
#' the avg_net_position function.
#'
agg_avg_net_position <- function(payments, debit, central_bank = NULL,
                                 t_start = NULL, t_end = NULL) {

  participants <- unique(payments$from)

  if(!is.null(central_bank)) {
    participants <- participants[-which(participants == central_bank)]
  }

  net_position_by_participant <-
    (mclapply(participants, function(x) avg_net_position(x, payments, debit,
                                                       t_start, t_end)))

  net_position_by_participant <-
    do.call("rbind", net_position_by_participant)


  net_position_by_participant <-
    net_position_by_participant[, .(aggregate_net = sum(avg_net_pos)),
                                by = list(date)]

  return(net_position_by_participant)
}
