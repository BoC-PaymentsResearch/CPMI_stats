
install.packages("devtools")

library(devtools)

#------------------------------------------------------------------------------

# If this is the first time running this script then run the following command

install_github("BoC-PaymentsResearch/CPMI_stats")

# This function call will install the CPMI_stats package from the github
# repository, and also all of the packages dependencies. This will take
# a few minutes to run

#------------------------------------------------------------------------------

# If you have previously used the install_github function then you should run
# the following command before calling any of the functions in the package

install_github("BoC-PaymentsResearch/CPMI_stats", dependencies = FALSE)

# This will ensure that you have the latest version of the package installed.
# Since you will have previouysly run the first install_github command you
# don't need to install the dependencies again so this call will save time.

library(CPMIstats)

# Example script for calling the functions

# For this example assuming that payments_data is
# a data frame of the correct format
# ID, date, time, value, from, to

# ID   - string
# date - string or R's date structure
# time - string with format "HH:MM:SS" (eg. 14:32:11)
# from - string
# to   - string

# All of the following should evalutate TRUE before calling the functions
class(payments_data$ID)   == "character"
class(payments_data$date) == "character" || class(payments_data$date) == "Date"
class(payments_data$time) == "character"
class(payments_data$value)== "numeric"
class(payments_data$from) == "character"
class(payments_data$to)   == "character"

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
