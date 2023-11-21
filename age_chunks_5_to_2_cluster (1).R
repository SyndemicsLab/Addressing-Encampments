### Age Chunks, 5 years to 2 years ###

# Last edited 04/06/2023
# Created by Hana Zwick, init_cohort code by Ryan O'Dea and adapted by Hana to
# other files

# RESPOND age categories are calibrated to 5-year chunks (ex. 10 - 14, 15 - 19,etc.)
# This script changes those to two-year age chunks (ex. 10- 11, 12 - 13, etc.)

# Don't forget to update user_inputs also!!!

# Necessary input sheets to update: initial cohort, all types od, background mortality, 
# block transitions, entering cohort, OUD trans, SMR, background utility,
# healthcare utilization cost

library(dplyr)

## set variables
arguments <- commandArgs(trailingOnly = TRUE)
input_number <- arguments[1]

#### initial cohort ####
init_cohort <- read.csv(paste0("input", input_number, "/init_cohort.csv"))

# split into rows, divide counts by 5, regroup
init_cohort <- init_cohort %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(block = unique(.$block), 
                           sex = unique(.$sex), 
                           oud = unique(.$oud), 
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(counts = counts/5,
         agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, block, sex, oud) %>% 
  summarise(counts = sum(counts))

# re-arrange
init_cohort <- init_cohort %>%
  arrange(factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox",
                                   "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")),
          agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", 
                                                    "Nonactive_Noninjection", "Nonactive_Injection"))) %>%
  select(block, agegrp, sex, oud, counts)

sum(init_cohort$counts)

write.csv(init_cohort, paste0("input", input_number, "/init_cohort.csv"), row.names = F)

#### all types overdose ####
all_types_od <- read.csv(paste0("input", input_number, "/all_types_overdose.csv"))

# take average of overdose rates
all_types_od <- all_types_od %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(block = unique(.$block), 
                           sex = unique(.$sex), 
                           oud = unique(.$oud), 
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, block, sex, oud) %>% 
  summarise(all_types_overdose_cycle52 = mean(all_types_overdose_cycle52),
            all_types_overdose_cycle104 = mean(all_types_overdose_cycle104),
            all_types_overdose_cycle156 = mean(all_types_overdose_cycle156))

# re-arrange
all_types_od <- all_types_od %>%
  arrange(factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox",
                                   "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")),
          agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", 
                                                    "Nonactive_Noninjection", "Nonactive_Injection"))) %>%
  select(block, agegrp, sex, oud, all_types_overdose_cycle52, all_types_overdose_cycle104, all_types_overdose_cycle156)

write.csv(all_types_od, paste0("input", input_number, "/all_types_overdose.csv"), row.names = F)

#### background mortality ####
background_mort <- read.csv(paste0("input", input_number, "/background_mortality.csv"))

# take average of death probability
background_mort <- background_mort %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(sex = unique(.$sex), 
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, sex) %>% 
  summarise(death_prob = mean(death_prob))

# re-arrange
background_mort <- background_mort %>%
  arrange(agegrp, desc(sex))

write.csv(background_mort, paste0("input", input_number, "/background_mortality.csv", sep = ""), row.names = F)

#### block transitions ####
block_trans <- read.csv(paste0("input", input_number, "/block_trans.csv"))

# take average of transition probabilities
block_trans <- block_trans %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(initial_block = unique(.$initial_block), 
                           sex = unique(.$sex), 
                           oud = unique(.$oud), 
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, initial_block, sex, oud) %>% 
  summarise(to_No_Treatment_cycle52 = mean(to_No_Treatment_cycle52),
            to_Buprenorphine_cycle52 = mean(to_Buprenorphine_cycle52),
            to_Naltrexone_cycle52 = mean(to_Naltrexone_cycle52),
            to_Methadone_cycle52 = mean(to_Methadone_cycle52),
            to_Detox_cycle52 = mean(to_Detox_cycle52),
            to_corresponding_post_trt_cycle52 = mean(to_corresponding_post_trt_cycle52),
            to_No_Treatment_cycle104 = mean(to_No_Treatment_cycle104),
            to_Buprenorphine_cycle104 = mean(to_Buprenorphine_cycle104),
            to_Naltrexone_cycle104 = mean(to_Naltrexone_cycle104),
            to_Methadone_cycle104 = mean(to_Methadone_cycle104),
            to_Detox_cycle104 = mean(to_Detox_cycle104),
            to_corresponding_post_trt_cycle104 = mean(to_corresponding_post_trt_cycle104),
            to_No_Treatment_cycle156 = mean(to_No_Treatment_cycle156),
            to_Buprenorphine_cycle156 = mean(to_Buprenorphine_cycle156),
            to_Naltrexone_cycle156 = mean(to_Naltrexone_cycle156),
            to_Methadone_cycle156 = mean(to_Methadone_cycle156),
            to_Detox_cycle156 = mean(to_Detox_cycle156),
            to_corresponding_post_trt_cycle156 = mean(to_corresponding_post_trt_cycle156))

# re-arrange
block_trans <- block_trans %>%
  arrange(factor(initial_block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox",
                                   "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")),
          agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", 
                                                    "Nonactive_Noninjection", "Nonactive_Injection"))) %>%
  relocate(agegrp, sex, oud, initial_block)

write.csv(block_trans, paste0("input", input_number, "/block_trans.csv"), row.names = F)

#### entering cohort ####
enter_cohort <- read.csv(paste0("input", input_number, "/entering_cohort.csv"))

# divide by 5, re-combine and sum each group of 2
enter_cohort <- enter_cohort %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(sex = unique(.$sex),  
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(number_of_new_comers_cycle52 = number_of_new_comers_cycle52/5,
         number_of_new_comers_cycle104 = number_of_new_comers_cycle104/5,
         number_of_new_comers_cycle156 = number_of_new_comers_cycle156/5,
         agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, sex) %>% 
  summarise(number_of_new_comers_cycle52 = sum(number_of_new_comers_cycle52),
            number_of_new_comers_cycle104 = sum(number_of_new_comers_cycle104),
            number_of_new_comers_cycle156 = sum(number_of_new_comers_cycle156))

sum(enter_cohort$number_of_new_comers_cycle52)

enter_cohort <- enter_cohort %>%
  arrange(agegrp, desc(sex))

write.csv(enter_cohort, paste0("input", input_number, "/entering_cohort.csv"), row.names = F)

#### OUD transitions ####
oud_trans <- read.csv(paste0("input", input_number, "/oud_trans.csv"))

# take the averages of each transition probability
oud_trans <- oud_trans %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(block = unique(.$block), 
                           sex = unique(.$sex), 
                           initial_status = unique(.$initial_status), 
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, sex, block, initial_status) %>% 
  summarise(to_Active_Noninjection = mean(to_Active_Noninjection),
            to_Active_Injection = mean(to_Active_Injection),
            to_Nonactive_Noninjection = mean(to_Nonactive_Noninjection),
            to_Nonactive_Injection = mean(to_Nonactive_Injection))

# re-arrange
oud_trans <- oud_trans %>%
  arrange(factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox",
                                           "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")),
          agegrp, desc(sex), factor(initial_status, levels = c("Active_Noninjection", "Active_Injection", 
                                                               "Nonactive_Noninjection", "Nonactive_Injection"))) %>%
  relocate(block)

write.csv(oud_trans, paste0("input", input_number, "/oud_trans.csv"), row.names = F)

#### SMR ####
SMR <- read.csv(paste0("input", input_number, "/SMR.csv"))

# take the average of the SMRs
SMR <- SMR %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(block = unique(.$block), 
                           sex = unique(.$sex), 
                           oud = unique(.$oud), 
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, block, sex, oud) %>% 
  summarise(SMR = mean(SMR))

# re-arrange
SMR <- SMR %>%
  arrange(factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox",
                                   "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")),
          agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", 
                                                    "Nonactive_Noninjection", "Nonactive_Injection"))) %>%
  select(block, agegrp, sex, oud, SMR)

write.csv(SMR, paste0("input", input_number, "/SMR.csv"), row.names = F)

#### background utility ####
bg_utility <- read.csv(paste0("input", input_number, "/cost_life/bg_utility.csv"))

# take the averages of the utilities
bg_utility <- bg_utility %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(sex = unique(.$sex), 
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, sex) %>% 
  summarise(utility = mean(utility))

bg_utility <- bg_utility %>%
  arrange(agegrp, desc(sex))

write.csv(bg_utility, paste0("input", input_number, "/cost_life/bg_utility.csv"), row.names = F)

#### healthcare utilization cost ####
healthcare_utilization_cost <- read.csv(paste0("input", input_number, "/cost_life/healthcare_utilization_cost.csv"))

# take the average of the costs
healthcare_utilization_cost <- healthcare_utilization_cost %>%
  mutate(age_min = as.numeric(sub("_(.*)", "", agegrp)),
         age_max = as.numeric(sub("^(.*?)_", "", agegrp))) %>% 
  select(-agegrp) %>% 
  full_join(., expand.grid(block = unique(.$block), 
                           sex = unique(.$sex), 
                           oud = unique(.$oud), 
                           age = as.numeric(c(min(.$age_min):max(.$age_max))))) %>% 
  filter(age >= age_min,
         age <= age_max) %>% 
  select(-c(age_min, age_max)) %>% 
  mutate(agegrp = cut(age, seq(9, 100, 2), labels = paste0(seq(10, 98, 2), "_", seq(11, 99, 2)))) %>% 
  group_by(agegrp, sex, block, oud) %>% 
  summarise(healthcare_utilization_cost_healthcare_system = mean(healthcare_utilization_cost_healthcare_system))

# re-arrange
healthcare_utilization_cost <- healthcare_utilization_cost %>%
  arrange(factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Detox",
                                   "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Detox")),
          agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", 
                                                    "Nonactive_Noninjection", "Nonactive_Injection"))) %>%
  relocate(block)

write.csv(healthcare_utilization_cost, paste0("input", input_number, "/cost_life/healthcare_utilization_cost.csv"), row.names = F)










