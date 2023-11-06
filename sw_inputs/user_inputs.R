### General inputs ###

input_number <- 2

# These are global variables, so all of them should have <<- instead of <-

# block names and order:
# no_treatment always is considered as trt0.
# treatment episodes would follow no_treatment. current order of trt: inpatient, outpatient
# then post_treatment episodes are added in the same order as treatments.

block <<- c("No_Treatment","Buprenorphine","Naltrexone", "Methadone", "Corrections", "section", "diaspora",
            "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone","Post-Corrections", "post-section", "post-diaspora")
            
# age brackets, always from youngest to oldest
agegrp <<- c("18_19", "20_21", "22_23", "24_25", "26_27", "28_29", "30_31", "32_33", "34_35", "36_37", "38_39", "40_41",
             "42_43", "44_45", "46_47", "48_49", "50_51", "52_53", "54_55", "56_57", "58_59", "60_61", "62_63", "64_65",
             "66_67", "68_69", "70_71", "72_73", "74_75", "76_77", "78_79", "80_81", "82_83", "84_85", "86_87", "88_89",
             "90_91", "92_93", "94_95", "96_97", "98_99")

#gender groups, "m" or "male" is the reference group, so it should always come first
sex <<- c("Male","Female")

# OUD states: active states always come first. Then add non_active states in the same order as active ones.
oud <<- c("Active_Noninjection","Active_Injection","Nonactive_Noninjection","Nonactive_Injection")

# Duration of simulation in cycles. remainder of duration/periods should be 0.
simulation_duration <<- 104

# Aging parameters
cycles_in_age_brackets <<- 5*52      # number of cycles in each age bracket. With weekly cycle and 5-year age bracket: 52*5

# entering cohort parameters
# mass cass is closed cohort
time_varying_entering_cohort_cycles <<- c(104)    # time intervals for entering cohort. Only the upper limit is inclusive.

# block transition parameters
time_varying_blk_trans_cycles <<- c(52, 53, 56, 65, 104) # each element should have its own matrix. Each of these matrices represents a time interval. Only the upper limit is inclusive.

# overdose parameters
# all types overdose and fatal to all types overdose ratios are considered to have the same time_varying intervals
time_varying_overdose_cycles <<- c(52, 53, 56, 65, 104)    # each element should have its own column(for all types) or row (for fatal). Each of them represents a time interval, from last cycle to current cycle. Only the upper limit is inclusive

# Indicate whether you want to include cost analysis here
cost_analysis <<- "yes"   # enter "no" for calibration mode
cost_perspectives <<- c("healthcare_system")  # cost perspectives to be included in cost analysis

# 3% discounting calculated for weekly cycle (.03/52 = ~ .0006)
discounting_rate <<- 0.0006
# OUTPUT OPTIONS
# ---------------------------------------------------------------------------------------------------------------------
# Number of cycles in desired intervals for combining outputs. E.g. annual interval with weekly cycles has period of 52.
# remainder of duration/periods should be 0.
# This parameter is also used for calculating accumulated cost/life
periods <<- 52

# For debugging purpose, if you need to reformat the output of the simulation to 1 file per block output, use the following flag
print_per_blk_output <<- "yes"      # "yes" or "no"

# Save general outputs (all compartments' sizes in each cycle), all types overdose, background mortality and admissions to treatment episodes as csv files
print_general_outputs <<- "yes"      # "yes" or "no"

# This function generates the general statistics which includes total size, number of males (sex_ID=1) and
# number of each OUD status of each block in chosen cycles.
# Input is a vector of cycles for general stats.
# If the input is an empty vector, e.g. c() with length of zero, this function will not be called.
general_stats_cycles <<- c(0:104)

# If "yes", all cost categories will be printed out and saved as .csv files.
# If cost_analysis is "yes" total cost per block per perspective per interval will be printed anyway.This option will provide additional outputs.
print_cost_categories <<- "yes"
