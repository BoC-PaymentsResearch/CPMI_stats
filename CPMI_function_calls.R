library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)

test_data <- "data/boc_ext08_t2_1711.txt"

test_data <- read_csv(test_data, col_names = F)

colnames(test_data) <-
  c("transId", "date", "cycle", "send_time",
    "settle_time", "value", "from", "to", "tranche")


test_data <- test_data %>%
  select(transId, date, send_time, value, from, to) %>%
  rename(time = send_time)

test_data$date <-
  as.Date(as.character(test_data$date), format = "%Y%m%d")




test_data2 <- test_data %>% filter(to != "BCANCA") %>% filter(time >= "06:00:00")




#
# test1 <- aggregate_net_position(1, debit = T, "sdfsdf", test_data, "sdfsdf")
#
# test2 <- aggregate_net_position(1, debit = F, "sdfsdf", test_data, "sdfsdf")
#
# test3 <- average_net_position("TDOMCA", 1, debit = T, "sdf", test_data, "sdf")
#
# test3 <- average_net_position("BOFMCA", 1, debit = F, "sdf", test_data, "sdf")
#
#
#
# # Max Liquidity Provision -----------------------------------------------------
#
# test5 <- max_intraday_liquidity_provision("TDOMCA", 1, test_data)
#
#
# # Get Interval ----------------------------------------------------------------
#
# test6 <- get_interval("07:05:36", 1)
#
#


# test_small2 <- test_small %>% mutate(interval = get_interval(send_time, 1)) %>%
#   group_by(interval) %>% summarise(sum_vals = sum(value))
#
# bnm <- cumsum(test_small2$sum_vals)