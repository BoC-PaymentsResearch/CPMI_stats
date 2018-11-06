
library(CPMIstats)

# This script gives an example of how these functions can be used to
# create a model

#------------------------------------------------------------------------------

# Inter Decile Range

deciles <- decile(example_data2)

# Need only the 10th and 90th percentile
deciles <- deciles[percentile == 10 | percentile == 90]

# Convert the times to numeric values
deciles$time <- time_to_seconds_from_midnight(deciles$time)

deciles <- deciles[, .(time_spread = time[2] - time[1]), by = .(date)]

#------------------------------------------------------------------------------

# Q measure

Q_measure <- sys_wide_liq_eff(example_data2)

#------------------------------------------------------------------------------

setkey(deciles, date)
setkey(Q_measure, date)

model_data <- merge(deciles, Q_measure, all = T)

model <- lm(efficiency ~ time_spread, data = model_data)
