
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
# Since you will have previously run the first install_github command you
# don't need to install the dependencies again so this call will save time.

library(CPMIstats)

# Example script for calling the functions

# For this example assuming that payments_data is
# a data frame of the correct format
# ID, date, time, value, from, to

## Update in version 0.1.3 the functions require the time column to be of class
#  hms, so before calling any of the functions run the command
payments_data$time <- as.hms(payments_data$time)

# ID   - string
# date - string or R's date structure
# time - hms object form "HH:MM:SS" (eg. 14:32:11)
# from - string
# to   - string

# All of the following should evaluate TRUE before calling the functions
class(payments_data$ID)   == "character"
class(payments_data$date) == "character" || class(payments_data$date) == "Date"
class(payments_data$time) == "hms" || class(payments_data$time) == "difftime"
class(payments_data$value)== "numeric"
class(payments_data$from) == "character"
class(payments_data$to)   == "character"



# The package contains synthetic data for one day in the correct format. This
# data is called example_payments, all of the following functions are called
# with this data.

# Liquidity --------------------------------------------------------------------

# Participant Level ----------

# daily maximum net debit position of participant i
max_debit <- max_liq_prov("BBBBBB", example_payments, T)


# average net debit position of participant i
avg_debit <- avg_net_position("BBBBBB", example_payments, T)


# daily maximum net credit position of participant i
max_credit <- max_liq_prov("BBBBBB", example_payments, F)


# average net credit position of participant i
avg_credit <- avg_net_position("BBBBBB", example_payments, F)

# liquidity provision relative to usage

# To improve performance liq_prov_rel_usage returns a closure. Before calculating
# the measure for a specific participant all of the aggregate level data is calculated
# which is stored in the environment of the closure returned by liq_prov_rel_usage.

# The closure then takes one argument: name of a participant

liq_prov_participant <- liq_prov_rel_usage(example_payments)

liq_prov <- liq_prov_participant("BBBBBB")

# System Level ----------

# average net debit position
agg_net_debit <- agg_avg_net_position(example_payments, T)

# average net credit position
agg_net_credit <- agg_avg_net_position(example_payments, F)

# liquidity efficiency
sys_liq_eff <- sys_wide_liq_eff(example_payments)

# liquidity provision relative to usage
agg_liq_prov <- agg_liq_prov_rel_usage(example_payments)

#------------------------------------------------------------------------------

# Timing ----------------------------------------------------------------------

# average payment timing
avg_time <- avg_payment_timing(example_payments)

# deciles
deciles <- decile(example_payments)

#------------------------------------------------------------------------------

# Distributional Considerations

# Gini coefficient
gini <- gini_coefficient(example_payments)
