# input file paths, status quo strategy

input_number <- 1

initial_cohort_file <<- paste("./input", input_number, "/init_cohort.csv", sep = '')
entering_cohort_file <<- paste("./input", input_number, "/entering_cohort.csv", sep = '')
oud_trans_file <<- paste("./input", input_number, "/oud_trans.csv", sep = '')
block_trans_file <<- paste("./input", input_number, "/block_trans.csv", sep = '')
block_init_effect_file <<- paste("./input", input_number, "/block_init_effect.csv", sep = '')
all_type_overdose_file <<- paste("./input", input_number, "/all_types_overdose.csv", sep = '')
fatal_overdose_file <<-  paste("./input", input_number, "/fatal_overdose.csv", sep = '')
background_mortality_file <<- paste("./input", input_number, "/background_mortality.csv", sep = '')
SMR_file <<- paste("./input", input_number, "/SMR.csv", sep = '')
healthcare_utilization_cost_file <<- paste("./input", input_number, "/cost_life/healthcare_utilization_cost.csv", sep = '')
overdose_cost_file <<- paste("./input", input_number, "/cost_life/overdose_cost.csv", sep = '')
treatment_utilization_cost_file <<- paste("./input", input_number, "/cost_life/treatment_utilization_cost.csv", sep = '')
pharmaceutical_cost_file <<- paste("./input", input_number, "/cost_life/pharmaceutical_cost.csv", sep = '')
background_utility_file <<- paste("./input", input_number, "/cost_life/bg_utility.csv", sep = '')
oud_utility_file <<- paste("./input", input_number, "/cost_life/oud_utility.csv", sep = '')
setting_utility_file <<- paste("./input", input_number, "/cost_life/setting_utility.csv", sep = '')
