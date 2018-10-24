library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)


library(CPMIstats)

# Example script for calling the functions

# For this example assuming that payments_data is
# a data frame of the correct format
# ID, date, time, value, from, to

# Liquidity --------------------------------------------------------------------

# Participant Level ----------

# daily maximum net debit position of participant i
max_debit <- max_liq_prov(i, payments_data, T)


# average net debit position of participant i
avg_debit <- avg_net_position(i, payments_data, T)


# daily maximum net credit position of participant i
max_credit <- max_liq_prov(i, payments_data, F)


# average net credit position of participant i
avg_credit <- avg_net_position(i, payments_data, F)

# liquidity provision relative to usage
liq_prov <- liq_prov_rel_usage(i, payments_data)

# System Level ----------

# average net debit position
agg_net_debit <- agg_avg_net_position(payments_data, T)

# average net credit position
agg_net_credit <- agg_avg_net_position(payments_data, F)

# liquidity efficiency
sys_liq_eff <- sys_wide_liq_eff(payments_data)

# liquidity provision relative to usage
agg_liq_prov <- agg_liq_prov_rel_usage(payments_data)

#------------------------------------------------------------------------------

# Timing ----------------------------------------------------------------------

# average payment timing
avg_time <- avg_payment_timing(payments_data)

# deciles
deciles <- decile(payments_data)

#------------------------------------------------------------------------------

# Distributional Considerations

# Gini coefficient
gini <- gini_coefficient(payments_data)
