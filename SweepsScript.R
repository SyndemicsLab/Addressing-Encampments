source("SweepsFunctions.R")
library(data.table)


for(i in 1:1e3){
  #Input Cohort================
  init <- change_agegrp_chunk(fread(paste0("input", i, "/init_cohort.csv")), 
                              2, "sum", "counts")
  write.csv(init, paste0("input", i, "/init_cohort.csv", row.names = FALSE))
  
  #All types Overdose ========
  od <- change_agegrp_chunk(fread(paste0("input", i, "/all_types_overdose.csv")),
                            2, "mean", paste0("all_types_overdose_cycle", c(52, 104, 156)))
  write.csv(od, paste0("input", i, "/all_types_overdose.csv"), row.names = FALSE)
  
  #Background Mortality ======
  bg <- change_agegrp_chunk(fread(paste0("input", i, "/background_mortality.csv")),
                            2, "mean", "death_prob")
  write.csv(bg, paste0("input", i, "/background_mortality.csv"), row.names = FALSE)
  
  #Block Transitions ========
  blktr <- change_agegrp_chunk(fread(paste0("input", i, "/block_trans.csv")),
                               2, "mean", as.vector(outer(c("to_No_Treatment_cycle",
                                                            "to_Buprenorphine_cycle",
                                                            "to_Methadone_cycle",
                                                            "to_Naltrexone_cycle",
                                                            "to_Detox_cycle",
                                                            "to_corresponding_post_trt_cycle"),
                                                          c(52, 104, 156), paste0)))
  write.csv(blktr, paste0("input", i, "/block_trans.csv"), row.names = FALSE)
  
  #Entering Cohort =========
  entr <- change_agegrp_chunk(fread(paste0("input", i, "/entering_cohort.csv")),
                             2, "sum", c(paste0("number_of_newcomers_cycle", c(52, 104, 156))))
  write.csv(entr, paste0("input", i, "/entering_cohort.csv"), row.names = FALSE)
  
  #OUD Transitions =========
  oudtr <- change_agegrp_chunk(fread(paste0("input", i, "/oud_trans.csv")), 
                               2, "mean", as.vector(outer(c("to_Active_", "to_Nonactive_"),
                                                          c("Injection", "Noninjection"), paste0)))
  write.csv(oudtr, paste0("input", i, "/oud_trans.csv"), row.names = FALSE)
  
  #SMRs ====================
  smr <- change_agegrp_chunk(fread(paste0("input", i, "/SMR.csv")), 
                             2, "mean", "SMR")
  write.csv(smr, paste0("input", i, "/SMR.csv"), row.names = FALSE)
  
  #Healthcare Utilization =
  hcutil <- change_agegrp_chunk(fread(paste0("input", i, "cost_life/healthcare_utilization.csv")), 
                                2, "mean", "healthcare_utilization_cost_healthcare_system")
  write.csv(hcutil, paste0("input", i, "cost_life/healthcare_utilization.csv"), row.names = FALSE)
  
  #Background Utilization =
  bgutil <- change_agegrp_chunk(fread(paste0("input", i, "cost_life/bg_utility.csv")),
                                2, "mean", "utility")
  write.csv(bgutil, paste0("input", i, "cost_life/bg_utility.csv"), row.names = FALSE)
  
}
