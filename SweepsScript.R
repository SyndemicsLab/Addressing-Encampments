source("SweepsFunctions.R")
library(data.table)

#Input Cohort================
for(i in 1:1e3){
  data <- fread(paste0("input", i, "/init_cohort.csv"))
  manip <- change_agegrp_chunk(data, 2, "sum", "counts")
  write.csv(manip, paste0("input", i, "/init_cohort.csv", row.names = FALSE))
}

#All types Overdose ========
for(i in 1:1e3){
  data <- fread(paste0("input", i, "/all_types_overdose.csv"))
  manip <- change_agegrp_chunk(data, 2, "mean", paste0("all_types_overdose_cycle", c(52, 104, 156)))
  write.csv(manip, paste0("input", i, "/all_types_overdose.csv"))
}