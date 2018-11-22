#' Average Net Position
#'
#' @param participant character value, Identifying code of the payments system
#'                    participant
#' @param payments dataframe value, payments data
#' @param debit logical value, if this value is TRUE then the net debit position
#'              is calculated. If FALSE then the net credit position is calculated.
#' @param t_start character value, First time in the day when payments are sent.
#'                Format is HH:MM:SS (ex. 03:09:23)
#'                Defaults to NULL, taking the time of the first payment in the
#'                data as it's value.
#' @param t_end character value, Last time in the day when payments are sent. Format
#'              is HH:MM:SS (ex. 03:09:23)
#'              Defaults to NULL, taking the time of the last payment in the
#'              data as it's value.
#'
#' @return dataframe structure, The average net debit/credit position of the given participant
#'
#' @details
#'
#' Assumes that the payments data file has the form:
#' ID, date, time, value, from, to
#'
avg_net_position <- function(participant, payments, debit,
                             t_start = NULL, t_end = NULL) {


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

  payments <- payments[from == participant | to == participant]

  if(debit) {
    payments[from != participant, value := -value]
  } else {
    payments[from == participant, value := -value]
  }

  net_payments <- payments[, .(net = sum(value)), keyby = .(date, time)]

  net_payments <- net_payments[, .(time = time, net = cumsum(net)), keyby = .(date)]


  # Converting payment times to the number of seconds from midnight
  net_payments[, time2 := time_to_seconds_from_midnight(time)]

  t0 <-
    ifelse(
      !is.null(t_start) ,
      as.numeric(substring(t_start, 1, 2)) * 60 ^ 2 +
        as.numeric(substring(t_start, 4, 5)) * 60 +
        as.numeric(substring(t_start, 7, 8)),
      min(net_payments$time2)
    )

  T_final <-
    ifelse(
      !is.null(t_end),
      as.numeric(substring(t_end, 1, 2)) * 60 ^ 2 +
        as.numeric(substring(t_end, 4, 5)) * 60 +
        as.numeric(substring(t_end, 7, 8)),
      max(net_payments$time2)
    )

  if(t0 == T_final) {
    warning("The final time and the initial time are the same, if you passed values
            to t_start and t_end check if they follow the correct format (HH:MM:SS).
            If you didn't pass anything to t_start and t_end check if the format of the
            time column in your data is as outlined in CPMI_calls.R")
  }

  net_payments[, dt := time2 - shift(time2), by = .(date)
               ][, dt := ifelse(.I == 1, first(time2) - t0, dt), by = .(date)
                 ][, Wt := (weighting(time2, T_final, t0)), by = .(date)
                   ][, dWt := (weighting(time2, T_final, t0) +
                       weighting(shift(time2), T_final, t0)) / 2, by = .(date)
                     ][, dWt := ifelse(.I == 1, (1 + Wt) / 2, dWt), by = .(date)
                       ][, wn := shift(net), by = .(date)
                         ][, wn := ifelse(.I == 1, 0, wn), by = .(date)]

  net_payments <-
    net_payments[, .(avg_net_pos = (2 / (T_final - t0)) *
                       sum(pmax(wn, 0, na.rm = T) * dt * dWt, na.rm = T)),
                 by = .(date)]

  net_payments[, avg_net_pos := avg_net_pos / 1.0e+09]

  return(net_payments)
}
