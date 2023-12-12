## Aggregate Results from Cluster Runs

# Last updated 12/11/2023 by Dimitri and Hana

# Outputs of interest: total fatal overdoses, total overdoses, cost, QALYs, and 
# weeks in housing

# Load libraries
library(dplyr)

# Use input sets 5411 to 6410
runs <- 1:1000
  # to test, change this to a set I have outputs for and use print()
  # to see what comes out

# number of overall runs (to be used for CIs)
sample_size <- runs[length(runs)] - runs[1] + 1

# read in this list of relevant output files
output_file_names <- c("general_stats1.csv", 
                       "cost_life/CE_outputs1.csv", 
                       "cost_life/total_costs1.csv",
                       "all_types_overdose1.csv")

## group files by run number

# empty list
grouped_runs <- list()

# output folder creates a folder for each run number
# current tables holds each table, indexed by run and file name
  # current tables is a temp variable
# cycle through all output files, read in the tables
# add the set of all tables to grouped runs to reference later

# grouped runs will have a list of output#, which in turn are lists with
# each csv in them

for (run in runs) {
  output_folder <- paste0("output", run)
  current_tables <- list()
  for (output_file in output_file_names) {
    current_tables[[output_file]] <- read.csv(paste0(output_folder, "/", output_file))
  }
  grouped_runs[[output_folder]] <- current_tables
}

# access a specific file (and column)
# grouped_runs[["output5311"]]$general_stats1[,"accumulated_fatal_od_cycle0"]


# *****************************************************************************

# Time in Housing

# for time in housing, we need total population in housing from general_stats1, sum of cycles
# columns = 0 (1) to 104 (105), row = housing_meds

# Loop
th <- numeric(1000)
n <- runs[1]
year2 <- 53:104
for (folder in grouped_runs) {
  folderName <- paste0("output", n)
  current_stats <- grouped_runs[[folderName]][["general_stats1.csv"]]
  th[((n - 1) %% length(th)) + 1] <- sum(rowSums(current_stats[8, paste0("total_size_cycle", year2)]))
  n <- n + 1
}

# vector of 1000 numbers
th

# calculate mean/95% CI and output those to a csv
mean_th <- mean(th)
sd_th <- sd(th)
error_th <- qnorm(0.975)*sd_th/sqrt(sample_size)
lower_th <- mean_th - error_th
upper_th <- mean_th + error_th

median_th <- quantile(th, probs = c(0.5))
lower_new_th <- quantile(th, probs = c(0.025))
upper_new_th <- quantile(th, probs = c(0.975))

median_400 <- median_th/507.19
lower_400 <- lower_new_th/507.19
upper_400 <- upper_new_th/507.19
sd_400 <- sd_th/507.19

time_housed_stats <- data.frame(median_400, sd_400, lower_400, upper_400, median_th, lower_new_th, upper_new_th)

write.csv(time_housed_stats, "time_housed_stats.csv", row.names=FALSE)

# *****************************************************************************

# All-Cause Mortality

# from general_stats1, we need sum @cycle0, sum @cycle, delta, and % loss

acm <- numeric(1000)
h <- runs[1]
for (folder in grouped_runs) {
  folderName <- paste0("output", h)
  current_stats <- grouped_runs[[folderName]][["general_stats1.csv"]]
  acm[((h - 1) %% length(acm)) + 1] <- (sum(current_stats[, "total_size_cycle52"]) - sum(current_stats[, "total_size_cycle104"]))
  h <- h + 1
}

# vector of 1000 numbers
# acm

# calculations
mean_acm <- mean(acm)

sd_acm <- sd(acm)
error_acm <- qnorm(0.975)*sd_acm/sqrt(sample_size)
lower_acm <- mean_acm - error_acm
upper_acm <- mean_acm + error_acm

median_acm <- quantile(acm, probs = c(0.5))
lower_new_acm <- quantile(acm, probs = c(0.025))
upper_new_acm <- quantile(acm, probs = c(0.975))

median_400 <- median_acm/507.19
lower_400 <- lower_new_acm/507.19
upper_400 <- upper_new_acm/507.19
sd_400 <- sd_acm/507.19

percent_dead <- acm/sum(current_stats[, "total_size_cycle0"])
avg_percent_dead <- mean(percent_dead)

all_cause_mortality_stats <- data.frame(median_400, sd_400, lower_400, upper_400, median_acm, lower_new_acm, upper_new_acm, avg_percent_dead)

write.csv(all_cause_mortality_stats, "all_cause_mortality_stats.csv", row.names=FALSE)


# *****************************************************************************

# Fatal Overdoses

# for fatal overdose, we need total fod from general_stats1, sum of cycles
# 0 (XH) to 104 (ABH) (all rows)

# code Hana started with
# fod <- sum(rowSums(grouped_runs[["output1"]]$general_stats1[632:736]))

# Hana's loop
fod <- numeric(1000)
i <- runs[1]
for (folder in grouped_runs) {
   folderName <- paste0("output", i)
   current_stats <- grouped_runs[[folderName]][["general_stats1.csv"]]
   fod[((i - 1) %% length(fod)) + 1] <- sum(rowSums(current_stats[, paste0("accumulated_fatal_od_cycle", year2)]))
#   fod[((i - 1) %% length(fod)) + 1] <- sum(rowSums(current_stats[, grepl("accumulated_fatal_od_cycle", names(current_stats))]))
   i <- i + 1
}

# vector of 1000 numbers
# fod 

# calculate mean/95% CI and output those to a csv
mean_fod <- mean(fod)
sd_fod <- sd(fod)
error_fod <- qnorm(0.975)*sd_fod/sqrt(sample_size)
lower_fod <- mean_fod - error_fod
upper_fod <- mean_fod + error_fod

median_fod <- quantile(fod, probs = c(0.5))
lower_new_fod <- quantile(fod, probs = c(0.025))
upper_new_fod <- quantile(fod, probs = c(0.975))

median_400 <- median_fod/507.19
lower_400 <- lower_new_fod/507.19
upper_400 <- upper_new_fod/507.19
sd_400 <- sd_fod/507.19

fatal_od_stats <- data.frame(median_400, sd_400, lower_400, upper_400, median_fod, lower_new_fod, upper_new_fod)

write.csv(fatal_od_stats, "fatal_od_stats.csv", row.names=FALSE)

# *****************************************************************************

# All Overdoses

# for total overdose, we need to use all_types_overdose1, sum A/2 to CZ/106
# (all rows)

# code Hana started with
# aod <- sum(rowSums(grouped_runs[["output1"]]$all_types_overdose1[1:104]))

# Hana's loop
aod <- numeric(1000)
j <- runs[1]

for (folder in grouped_runs) {
  folderName <- paste0("output", j)
  current_stats <- grouped_runs[[folderName]]$all_types_overdose1
  aod[((j - 1) %% length(aod)) + 1] <- sum(rowSums(current_stats[, 54:104]))
#  aod[((j - 1) %% length(aod)) + 1] <- sum(rowSums(current_stats[, grepl("r", names(current_stats))]))
  j <- j +1
}

# vector of 1000 numbers
# aod

# calculate mean/95% CI and output those to a csv
mean_aod <- mean(aod)
sd_aod <- sd(aod)
error_aod <- qnorm(0.975)*sd_aod/sqrt(sample_size)
lower_aod <- mean_aod - error_aod
upper_aod <- mean_aod + error_aod

median_aod <- quantile(aod, probs = c(0.5))
lower_new_aod <- quantile(aod, probs = c(0.025))
upper_new_aod <- quantile(aod, probs = c(0.975))

median_400 <- median_aod/507.19
lower_400 <- lower_new_aod/507.19
upper_400 <- upper_new_aod/507.19

all_od_stats <- data.frame(median_400, lower_400, upper_400, median_aod, lower_new_aod, upper_new_aod)

write.csv(all_od_stats, "all_od_stats.csv", row.names=FALSE)


# *****************************************************************************

# Costs

# for cost, we need CE_outputs1.csv, B2 (total healthcare systems cost)
# no summing necessary, just pull the value

# code Hana started with
# grouped_runs[["output1"]]$"cost_life/CE_outputs1.csv"[1,"total"]

# Hana's loop
cost <- numeric(1000)
k <- runs[1]

for (folder in grouped_runs) {
  folderName <- paste0("output", k)
  current_stats <- grouped_runs[[folderName]][["cost_life/total_costs1.csv"]]
  cost[((k - 1) %% length(cost)) + 1] <- rowSums(current_stats[2, ])
#  cost[((k - 1) %% length(cost)) + 1] <- grouped_runs[[folderName]]$"cost_life/CE_outputs1.csv"[1, "total"]
  k <- k +1
}

# vector of 1000 numbers
# cost

# calculate mean/95% CI and output those to a csv
mean_cost <- mean(cost)
sd_cost <- sd(cost)
error_cost <- qnorm(0.975)*sd_cost/sqrt(sample_size)
lower_cost <- mean_cost - error_cost
upper_cost <- mean_cost + error_cost

median_cost <- quantile(cost, probs = c(0.5)) + 7187301.41 # sweep one-time cost based on wk 53 pop of 28,431.72
lower_new_cost <- quantile(cost, probs = c(0.025)) + 7187301.41 # sweep one-time cost based on wk 53 pop of 28,431.72
upper_new_cost <- quantile(cost, probs = c(0.975)) + 7187301.41 # sweep one-time cost based on wk 53 pop of 28,431.72

median_400 <- median_cost/507.19
lower_400 <- lower_new_cost/507.19
upper_400 <- upper_new_cost/507.19

cost_stats <- data.frame(median_400, lower_400, upper_400, median_cost, lower_new_cost, upper_new_cost)

write.csv(cost_stats, "cost_stats.csv", row.names=FALSE)


# *****************************************************************************

# QALYs

# need cost_life/CE_outputs#.csv
# columns: total, discounted_total
# rows: utility_minimal

# value / number of cycles / number of people at start

# Hana's loop
qaly <- numeric(1000)
m <- runs[1]

for (folder in grouped_runs) {
  folderName <- paste0("output", m)
  qaly[((m - 1) %% length(qaly)) + 1] <- grouped_runs[[folderName]]$"cost_life/CE_outputs1.csv"[3, "total"]
  m <- m +1
}

# vector of 1000 numbers
# qaly

# divided by cycles
qaly <- qaly/104

# divided by sum of people
qaly <- qaly/29136 # is there a way to automate this?

# now we have qaly per person per cycle?

# calculate mean/95% CI and output those to a csv
mean_qaly <- mean(qaly)
sd_qaly <- sd(qaly)
error_qaly <- qnorm(0.975)*sd_qaly/sqrt(sample_size)
lower_qaly <- mean_qaly - error_qaly
upper_qaly <- mean_qaly + error_qaly

median_qaly <- quantile(qaly, probs = c(0.5))
lower_new_qaly <- quantile(qaly, probs = c(0.025))
upper_new_qaly <- quantile(qaly, probs = c(0.975))

qaly_stats <- data.frame(mean_qaly, lower_qaly, upper_qaly, median_qaly, lower_new_qaly, upper_new_qaly)

write.csv(qaly_stats, "qaly_stats.csv", row.names=FALSE)

# *****************************************************************************

# Time in MOUD

# for time in MOUD, we need total population in bup/ntx/mmt/housing from general_stats1, sum of cycles
# columns = 53 to 104, rows = Buprenorphine/Naltrexone/Methadone/housing_meds

# Loop
tm <- numeric(1000)
p <- runs[1]
for (folder in grouped_runs) {
  folderName <- paste0("output", p)
  current_stats <- grouped_runs[[folderName]][["general_stats1.csv"]]
  tm[((p - 1) %% length(tm)) + 1] <- sum(rowSums(current_stats[2, paste0("total_size_cycle", year2)])) + sum(rowSums(current_stats[3, paste0("total_size_cycle", year2)])) + sum(rowSums(current_stats[4, paste0("total_size_cycle", year2)])) + sum(rowSums(current_stats[8, paste0("total_size_cycle", year2)]))
#  tm[((p - 1) %% length(tm)) + 1] <- sum(rowSums(current_stats[2:4, paste0("total_size_cycle", year2)]))
#  tm[((p - 1) %% length(tm)) + 1] <- sum(rowSums(current_stats[2, grepl("total_size_cycle", names(current_stats))])) + sum(rowSums(current_stats[3, grepl("total_size_cycle", names(current_stats))])) + sum(rowSums(current_stats[4, grepl("total_size_cycle", names(current_stats))])) + sum(rowSums(current_stats[8, grepl("total_size_cycle", names(current_stats))]))
  p <- p + 1
}

# vector of 1000 numbers
tm

# calculate mean/95% CI and output those to a csv
mean_tm <- mean(tm)
sd_tm <- sd(tm)
error_tm <- qnorm(0.975)*sd_tm/sqrt(sample_size)
lower_tm <- mean_tm - error_tm
upper_tm <- mean_tm + error_tm

median_tm <- quantile(tm, probs = c(0.5))
lower_new_tm <- quantile(tm, probs = c(0.025))
upper_new_tm <- quantile(tm, probs = c(0.975))

median_400 <- median_tm/507.19
lower_400 <- lower_new_tm/507.19
upper_400 <- upper_new_tm/507.19
sd_400 <- sd_tm/507.19

time_moud_stats <- data.frame(median_400, sd_400, lower_400, upper_400, median_tm, lower_new_tm, upper_new_tm)

write.csv(time_moud_stats, "time_moud_stats.csv", row.names=FALSE)

# *****************************************************************************

print("ran with no errors")

# *****************************************************************************

## General Helpful Notes

# gets total sum per row
# rowSums()

# use sum() around the rowSums() to get one number (total)
# eventually we will have one number per output folder, use those for statistics

# next steps
 # look at one output folder, make sure numbers are coming out as expected
 # if yes, make similar loops to get those sums
 # reach out for help with storing them somewhere (use a vector (c()))

# access a specific column
# print(grouped_runs[["output1"]][["general_stats1.csv"]][, "accumulated_admissions_cycle104"])

# pulls the name of every column containing accumulated_fatal_od_cycle
# grepl("accumulated_fatal_od_cycle", 
      # names(grouped_runs[["output5311"]][["general_stats1.csv"]]))

# subtable, can do a sum across the entire thing
# print(grouped_runs[["output5311"]][["general_stats1.csv"]][, grepl("accumulated_fatal_od_cycle", names(grouped_runs[["output5311"]][["general_stats1.csv"]]))])


