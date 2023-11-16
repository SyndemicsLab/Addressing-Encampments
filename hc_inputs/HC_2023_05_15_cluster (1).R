# Mass Cass Sweeps Project

## Script for Status Quo scenario
## Author: Hana Zwick
##  Last updated 04/25/2023

# This version runs on the computer with a single set of parameter inputs, but
# is closer to the cluster in that it reads an ec base case file and then 
# manipulates it and writes it to the input# file, with no shell tables used.

# Idealistic scenario, MOUD choice
# This is the model with transitional housing at week 53
# Two housing blocks: with and without meds





# getwd()

## libraries
library(dplyr)
library(SciViews)

## set variables
arguments <- commandArgs(trailingOnly = TRUE)
input_number <- arguments[1]


##### PROBABILITIES ########################################################

### initial cohort ##########

init_cohort <- read.csv(paste0("input", input_number, "/init_cohort.csv"))

init_cohort <- init_cohort %>%
  filter(block != "Detox") %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

new_corr <- init_cohort %>%
  filter(block == "No_Treatment")
new_corr[, c(5)] <- rep(0, length.out = 328)
new_corr$block <- "Corrections"

new_section <- init_cohort %>%
  filter(block == "No_Treatment")
new_section[, c(5)] <- rep(0, length.out = 328)
new_section$block <- "section"

new_diasp <- init_cohort %>%
  filter(block == "No_Treatment")
new_diasp[, c(5)] <- rep(0, length.out = 328)
new_diasp$block <- "diaspora"

new_housing_meds <- init_cohort %>%
  filter(block == "No_Treatment")
new_housing_meds[, c(5)] <- rep(0, length.out = 328)
new_housing_meds$block <- "housing_meds"

new_housing_nomeds <- init_cohort %>%
  filter(block == "No_Treatment")
new_housing_nomeds[, c(5)] <- rep(0, length.out = 328)
new_housing_nomeds$block <- "housing_nomeds"

init_cohort <- init_cohort %>%
  rbind(new_corr, new_section, new_diasp, new_housing_meds, new_housing_nomeds)

init_cohort <- init_cohort %>%
  arrange(
    factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora", "housing_meds", "housing_nomeds")),
    agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

write.csv(init_cohort, paste0("input", input_number, "/init_cohort.csv"), row.names = F)

### all types OD ###########

all_types_od <- read.csv(paste0("input", input_number, "/all_types_overdose.csv"))

# add new time cycles, limit age
all_types_od <- all_types_od %>%
  mutate(all_types_overdose_cycle53 = all_types_overdose_cycle156) %>%
  mutate(all_types_overdose_cycle56 = all_types_overdose_cycle156) %>%
  mutate(all_types_overdose_cycle65 = all_types_overdose_cycle156) %>%
  select(-all_types_overdose_cycle156) %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

# add new blocks
corr_aod <- all_types_od %>%
  filter(block == "No_Treatment")
corr_aod$block <- "Corrections"
  
sect_aod <- all_types_od %>%
  filter(block == "No_Treatment")
sect_aod$block <- "section"

diasp_aod <- all_types_od %>%
  filter(block == "No_Treatment")
diasp_aod$block <- "diaspora"

housing_aod <- all_types_od %>%
  filter(block == "No_Treatment")
housing_aod$block <- "housing_meds"

housing_nm_aod <- all_types_od %>%
  filter(block == "No_Treatment")
housing_nm_aod$block <- "housing_nomeds"

p_corr_aod <- all_types_od %>%
  filter(block == "No_Treatment")
p_corr_aod$block <- "Post-Corrections"

p_sect_aod <- all_types_od %>%
  filter(block == "Post-Buprenorphine")
p_sect_aod$block <- "post-section"

p_diasp_aod <- all_types_od %>%
  filter(block == "No_Treatment")
p_diasp_aod$block <- "post-diaspora"

p_housing_aod <- all_types_od %>%
  filter(block == "No_Treatment")
p_housing_aod$block <- "post-housing_meds"

p_housing_nm_aod <- all_types_od %>%
  filter(block == "No_Treatment")
p_housing_nm_aod$block <- "post-housing_nomeds"

all_types_od <- rbind(all_types_od, corr_aod, sect_aod, diasp_aod, housing_aod, housing_nm_aod,
                      p_corr_aod, p_sect_aod, p_diasp_aod, p_housing_aod, p_housing_nm_aod)

#arrange
all_types_od <- all_types_od %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox") %>%
  arrange(
    factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora", "housing_meds", "housing_nomeds",
                             "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "post-section", "post-diaspora", "post-housing_meds", "post-housing_nomeds")),
    agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))) %>%
  select("block", "agegrp", "sex", "oud", "all_types_overdose_cycle52", "all_types_overdose_cycle53", "all_types_overdose_cycle56", 
         "all_types_overdose_cycle65", "all_types_overdose_cycle104")

## No_Treatment
all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

## Buprenorphine
all_types_od[all_types_od$block == "Buprenorphine", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Buprenorphine", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Buprenorphine", "all_types_overdose_cycle65"]

## Naltrexone
all_types_od[all_types_od$block == "Naltrexone", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Naltrexone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Naltrexone", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Naltrexone", "all_types_overdose_cycle65"]

## Methadone
all_types_od[all_types_od$block == "Methadone", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Methadone", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Methadone", "all_types_overdose_cycle65"]

## Corrections
all_types_od[all_types_od$block == "Corrections", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"] * 0.5

all_types_od[all_types_od$block == "Corrections", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"] * 0.5

all_types_od[all_types_od$block == "Corrections", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"] * 0.5

all_types_od[all_types_od$block == "Corrections", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"] * 0.5

## section = 0
all_types_od[all_types_od$block == "section", "all_types_overdose_cycle52"] <- 0

all_types_od[all_types_od$block == "section", "all_types_overdose_cycle53"] <- 0

all_types_od[all_types_od$block == "section", "all_types_overdose_cycle56"] <- 0

all_types_od[all_types_od$block == "section", "all_types_overdose_cycle65"] <- 0

all_types_od[all_types_od$block == "section", "all_types_overdose_cycle104"] <- 0

## diaspora = No Treatment
all_types_od[all_types_od$block == "diaspora", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "diaspora", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "diaspora", "all_types_overdose_cycle56"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "diaspora", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "diaspora", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

## housing_meds = methadone
all_types_od[all_types_od$block == "housing_meds", "all_types_overdose_cycle52"] <- 
  all_types_od[all_types_od$block == "Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "housing_meds", "all_types_overdose_cycle53"] <- 
  all_types_od[all_types_od$block == "Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "housing_meds", "all_types_overdose_cycle65"] <- 
  all_types_od[all_types_od$block == "Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "housing_meds", "all_types_overdose_cycle104"] <- 
  all_types_od[all_types_od$block == "Methadone", "all_types_overdose_cycle65"]

## housing_nomeds = No Treatment
all_types_od[all_types_od$block == "housing_nomeds", "all_types_overdose_cycle52"] <- 
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "housing_nomeds", "all_types_overdose_cycle53"] <- 
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "housing_nomeds", "all_types_overdose_cycle65"] <- 
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "housing_nomeds", "all_types_overdose_cycle104"] <- 
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

## post Buprenorphine
all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

## post Methadone
all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

## post Naltrexone
all_types_od[all_types_od$block == "Post-Naltrexone", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Post-Naltrexone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Naltrexone", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "Post-Naltrexone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Naltrexone", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "Post-Naltrexone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Naltrexone", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Post-Naltrexone", "all_types_overdose_cycle65"]

## post Corrections
all_types_od[all_types_od$block == "Post-Corrections", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Corrections", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Corrections", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "Post-Corrections", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

## post-section = Post-Bup
all_types_od[all_types_od$block == "post-section", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-section", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-section", "all_types_overdose_cycle56"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-section", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-section", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Post-Buprenorphine", "all_types_overdose_cycle65"]

## post-diaspora = No Treatment
all_types_od[all_types_od$block == "post-diaspora", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-diaspora", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-diaspora", "all_types_overdose_cycle56"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-diaspora", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-diaspora", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

## post housing_meds = post-mmt
all_types_od[all_types_od$block == "post-housing_meds", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-housing_meds", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-housing_meds", "all_types_overdose_cycle56"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-housing_meds", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-housing_meds", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "Post-Methadone", "all_types_overdose_cycle65"]

## post housing_nomeds = No Treatment (this will be skipped over)
all_types_od[all_types_od$block == "post-housing_nomeds", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-housing_nomeds", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-housing_nomeds", "all_types_overdose_cycle56"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-housing_nomeds", "all_types_overdose_cycle65"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

all_types_od[all_types_od$block == "post-housing_nomeds", "all_types_overdose_cycle104"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"]

write.csv(all_types_od, paste0("input", input_number, "/all_types_overdose.csv"), row.names = F)

### background mortality #############

background_mort <- read.csv(paste0("input", input_number, "/background_mortality.csv"))

# don't change - this is the denominator for SMR, so the general population

background_mort <- background_mort %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(background_mort, paste0("input", input_number, "/background_mortality.csv", sep = ""), row.names = F)

### block initiation effect ##############

block_init_effect <- read.csv(paste0("input", input_number, "/block_init_effect.csv"))

block_init_effect <- block_init_effect %>% 
  mutate(
  to_section = 1, to_diaspora = 1, to_housing_meds = 1, to_Corrections = 1, to_housing_nomeds = 1,
  to_post.section = 1, to_post.diaspora = 1, to_post.housing_meds = 1, to_Post.Corrections = 1, to_post.housing_nomeds = 1
)
# post corrections = shift to active
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Noninjection", "to_Post.Corrections"] <- 0.4
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Injection", "to_Post.Corrections"] <- 0.4

# post section = shift to active
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Noninjection", "to_post.section"] <- 0.4
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Injection", "to_post.section"] <- 0.4

# post housing_meds = post mmt
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Noninjection", "to_post.housing_meds"] <- 
  block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Noninjection", "to_Post.Methadone"]
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Injection", "to_post.housing_meds"] <- 
  block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Injection", "to_Post.Methadone"]

# clean
block_init_effect <- block_init_effect %>% 
  select("initial_oud_state", "to_No_Treatment", "to_Buprenorphine", "to_Naltrexone",
         "to_Methadone", "to_Corrections", "to_section", "to_diaspora", "to_housing_meds", "to_housing_nomeds",
         "to_Post.Buprenorphine", "to_Post.Naltrexone", "to_Post.Methadone", "to_Post.Corrections", 
         "to_post.section", "to_post.diaspora", "to_post.housing_meds", "to_post.housing_nomeds")

write.csv(block_init_effect, paste0("input", input_number, "/block_init_effect.csv"), row.names = F)

### Block Transitions ############

block_trans <- read.csv(paste0("input", input_number, "/block_trans.csv"))
# DC_base_block_trans <- read.csv("../../../Model Documentation/Base Case/2021.3.25 Base Case/input1/block_trans.csv")

# add in blocks (rows)
corr_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
corr_bt[, c(5:22)] <- rep(0, length.out = 360)
corr_bt$initial_block <- "Corrections"

p_corr_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
p_corr_bt[, c(5:22)] <- rep(0, length.out = 360)
p_corr_bt$initial_block <- "Post-Corrections"

sect_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
sect_bt[, c(5:22)] <- rep(0, length.out = 360)
sect_bt$initial_block <- "section"

p_sect_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
p_sect_bt[, c(5:22)] <- rep(0, length.out = 360)
p_sect_bt$initial_block <- "post-section"

diasp_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
diasp_bt[, c(5:22)] <- rep(0, length.out = 360)
diasp_bt$initial_block <- "diaspora"

p_diasp_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
p_diasp_bt[, c(5:22)] <- rep(0, length.out = 360)
p_diasp_bt$initial_block <- "post-diaspora"

housing_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
housing_bt[, c(5:22)] <- rep(0, length.out = 360)
housing_bt$initial_block <- "housing_meds"

p_housing_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
p_housing_bt[, c(5:22)] <- rep(0, length.out = 360)
p_housing_bt$initial_block <- "post-housing_meds"

housing_nm_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
housing_nm_bt[, c(5:22)] <- rep(0, length.out = 360)
housing_nm_bt$initial_block <- "housing_nomeds"

p_housing_nm_bt <- block_trans %>%
  filter(initial_block == "No_Treatment")
p_housing_nm_bt[, c(5:22)] <- rep(0, length.out = 360)
p_housing_nm_bt$initial_block <- "post-housing_nomeds"

block_trans <- block_trans %>%
  rbind(corr_bt, p_corr_bt, sect_bt, p_sect_bt, diasp_bt, p_diasp_bt, 
        housing_bt, p_housing_bt, housing_nm_bt, p_housing_nm_bt)

# remove Detox, Post-Detox (rows)
block_trans <- block_trans %>%
  filter(initial_block != "Detox") %>%
  filter(initial_block != "Post-Detox")

# arrange to the correct row order

block_trans <- block_trans %>%
  arrange(agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection")), 
          factor(initial_block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora", "housing_meds", "housing_nomeds",
                                           "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "post-section", "post-diaspora",
                                           "post-housing_meds","post-housing_nomeds")))

# add new blocks (columns)

# 52
block_trans <- block_trans %>%
  mutate(
    to_Corrections_cycle52 = 0,
    to_section_cycle52 = 0,
    to_diaspora_cycle52 = 0,
    to_housing_meds_cycle52 = 0,
    to_housing_nomeds_cycle52 = 0
  )

# 53
block_trans <- block_trans %>%
  mutate(
    to_No_Treatment_cycle53 = 0,
    to_Buprenorphine_cycle53 = 0,
    to_Naltrexone_cycle53 = 0,
    to_Methadone_cycle53 = 0,
    to_Corrections_cycle53 = 0,
    to_section_cycle53 = 0,
    to_diaspora_cycle53 = 0,
    to_housing_meds_cycle53 = 0,
    to_housing_nomeds_cycle53 = 0,
    to_corresponding_post_trt_cycle53 = 0
  )

# 56
block_trans <- block_trans %>%
  mutate(
    to_No_Treatment_cycle56 = 0,
    to_Buprenorphine_cycle56 = 0,
    to_Naltrexone_cycle56 = 0,
    to_Methadone_cycle56 = 0,
    to_Corrections_cycle56 = 0,
    to_section_cycle56 = 0,
    to_diaspora_cycle56 = 0,
    to_housing_meds_cycle56 = 0,
    to_housing_nomeds_cycle56 = 0,
    to_corresponding_post_trt_cycle56 = 0
  )

# 65
block_trans <- block_trans %>%
  mutate(
    to_No_Treatment_cycle65 = 0,
    to_Buprenorphine_cycle65 = 0,
    to_Naltrexone_cycle65 = 0,
    to_Methadone_cycle65 = 0,
    to_Corrections_cycle65 = 0,
    to_section_cycle65 = 0,
    to_diaspora_cycle65 = 0,
    to_housing_meds_cycle65 = 0,
    to_housing_nomeds_cycle65 = 0,
    to_corresponding_post_trt_cycle65 = 0
  )

# 104
block_trans <- block_trans %>%
  mutate(
    to_Corrections_cycle104 = 0,
    to_section_cycle104 = 0,
    to_diaspora_cycle104 = 0,
    to_housing_meds_cycle104 = 0,
    to_housing_nomeds_cycle104 = 0
  )

# re-arrange columns using select(), remove Detox/Post-Detox by doing so
block_trans <- block_trans %>%
  select(agegrp, sex, oud, initial_block, 
         to_No_Treatment_cycle52, to_Buprenorphine_cycle52, to_Naltrexone_cycle52, to_Methadone_cycle52, to_Corrections_cycle52, to_section_cycle52, to_diaspora_cycle52, to_housing_meds_cycle52, to_housing_nomeds_cycle52, to_corresponding_post_trt_cycle52,
         to_No_Treatment_cycle53, to_Buprenorphine_cycle53, to_Naltrexone_cycle53, to_Methadone_cycle53, to_Corrections_cycle53, to_section_cycle53, to_diaspora_cycle53, to_housing_meds_cycle53, to_housing_nomeds_cycle53, to_corresponding_post_trt_cycle53,
         to_No_Treatment_cycle56, to_Buprenorphine_cycle56, to_Naltrexone_cycle56, to_Methadone_cycle56, to_Corrections_cycle56, to_section_cycle56, to_diaspora_cycle56, to_housing_meds_cycle56, to_housing_nomeds_cycle56, to_corresponding_post_trt_cycle56,
         to_No_Treatment_cycle65, to_Buprenorphine_cycle65, to_Naltrexone_cycle65, to_Methadone_cycle65, to_Corrections_cycle65, to_section_cycle65, to_diaspora_cycle65, to_housing_meds_cycle65, to_housing_nomeds_cycle65, to_corresponding_post_trt_cycle65,
         to_No_Treatment_cycle104, to_Buprenorphine_cycle104, to_Naltrexone_cycle104, to_Methadone_cycle104, to_Corrections_cycle104, to_section_cycle104, to_diaspora_cycle104, to_housing_meds_cycle104, to_housing_nomeds_cycle104, to_corresponding_post_trt_cycle104,
         to_No_Treatment_cycle156, to_Buprenorphine_cycle156, to_Naltrexone_cycle156, to_Methadone_cycle156, to_corresponding_post_trt_cycle156)

# limit age
block_trans <- block_trans %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

## No_Treatment

# to_Corrections, all cycles. do this first because it all has to be put in 
# manually and is used to calculate No_Treatment -> other blocks.

# Male 18_19
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "18_19" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
            "to_Corrections_cycle52"] <- 0.002319461
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "18_19" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
            "to_Corrections_cycle52"] <- 0.001644522

# Male 20_21, 22_23
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "20_21" | block_trans$agegrp == "22_23") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 0.005798652
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "20_21" | block_trans$agegrp == "22_23") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 0.004111304

# Male 24_25
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "24_25" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
            "to_Corrections_cycle52"] <- (0.005798652 + 0.006569522)/2
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "24_25" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
            "to_Corrections_cycle52"] <- (0.004111304 + 0.002295478)/2

# Male 26_27, 28_29
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "26_27" | block_trans$agegrp == "28_29") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 0.006569522
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "26_27" | block_trans$agegrp == "28_29") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 0.002295478

# Female 18_19
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "18_19" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
            "to_Corrections_cycle52"] <- 0.000376765
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "18_19" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
            "to_Corrections_cycle52"] <- 0.00026713

# Female 20_21, 22_23
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "20_21" | block_trans$agegrp == "22_23") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 0.000941913
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "20_21" | block_trans$agegrp == "22_23") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 0.000667826

# Female 24_25
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "24_25" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
            "to_Corrections_cycle52"] <- (0.000941913 + 0.00106713)/2
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "24_25" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
            "to_Corrections_cycle52"] <- (0.000667826 + 0.00037287)/2

# Female 26_27, 28_29
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "26_27" | block_trans$agegrp == "28_29") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 0.00106713
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "26_27" | block_trans$agegrp == "28_29") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 0.00037287


# other cycles = 52
block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle53"] <- block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle56"] <- block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle65"] <- block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle104"] <- block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]


# 52
# no treatment -> bup, ntx, and mmt are calibrated

block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle52"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"])

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 5:14]))

# 53
# CHANGE PROPORTIONS MOVING TO SECTION/DIASPORA
block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle53"] <- 0
# MOUD -> housing with meds
block_trans[block_trans$initial_block == "No_Treatment", "to_housing_nomeds_cycle53"] <- 0.675201
# No Treatment -> housing without meds
# 0.7087 - 0.1907(all MOUD moves to housing) = 0.518 left
# 51.8% of the entire pop at cycle 53 = 13,617.25
# 13,617.25 = 67.5201% of those in No Treatment
block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle53"] <- 
  1 - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_nomeds_cycle53"] - 
  block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle53"]
block_trans[block_trans$initial_block == "No_Treatment", "to_section_cycle53"] <- 0

block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle53"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle53"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle53"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle53"] <- 0

block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle53"] <- 0

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 15:24]))

# 56
# DROP FROM 80% ACCEPTANCE RATE
block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle56"] <- 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle52"] +
    block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle52"] + 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_housing_nomeds_cycle56"] <- 0.007015625
block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle56"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_section_cycle56"] <- 0

block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle56"] <- 
  block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle56"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle56"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle56"] <- 0

block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle56"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle56"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle56"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle56"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle56"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_section_cycle56"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle56"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle56"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_nomeds_cycle56"])

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 25:34]))

# 65
block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle65"] <- 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle52"] +
    block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle52"] + 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_housing_nomeds_cycle65"] <- 0.007015625
block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle65"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_section_cycle65"] <- 0

block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle65"] <- 
  block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle65"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle65"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle65"] <- 0

block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle65"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle65"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle65"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle65"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle65"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_section_cycle65"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle65"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle65"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_nomeds_cycle65"])

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 35:44]))

# 104

block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle104"] <- 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle52"] +
    block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle52"] + 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_housing_nomeds_cycle104"] <- 0.007015625
block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle104"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_section_cycle104"] <- 0

block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle104"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle104"] <- 0
block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle104"] <- 0

block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle104"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle104"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle104"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle104"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle104"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_section_cycle104"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle104"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle104"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_nomeds_cycle104"])

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 45:54]))

## Buprenorphine

# 52
# already calibrated

# 53
# nobody to section, some to housing, some to post-state instead of diaspora

block_trans[block_trans$initial_block == "Buprenorphine", "to_section_cycle53"] <- 0
block_trans[block_trans$initial_block == "Buprenorphine", "to_diaspora_cycle53"] <- 0
block_trans[block_trans$initial_block == "Buprenorphine", "to_housing_meds_cycle53"] <- 1
block_trans[block_trans$initial_block == "Buprenorphine", "to_housing_nomeds_cycle53"] <- 0
block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle53"] <- 0

block_trans[block_trans$initial_block == "Buprenorphine", "to_corresponding_post_trt_cycle53"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Buprenorphine", 15:24]))

# 56
block_trans[block_trans$initial_block == "Buprenorphine", "to_housing_meds_cycle56"] <- 1
block_trans[block_trans$initial_block == "Buprenorphine", "to_housing_nomeds_cycle56"] <- 0
block_trans[block_trans$initial_block == "Buprenorphine", "to_corresponding_post_trt_cycle56"] <- 0

block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle56"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Buprenorphine", 25:34]))

# 65
block_trans[block_trans$initial_block == "Buprenorphine", "to_housing_meds_cycle65"] <- 1
block_trans[block_trans$initial_block == "Buprenorphine", "to_housing_nomeds_cycle65"] <- 0
block_trans[block_trans$initial_block == "Buprenorphine", "to_corresponding_post_trt_cycle65"] <- 0

block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle65"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Buprenorphine", 35:44]))

# 104
block_trans[block_trans$initial_block == "Buprenorphine", "to_housing_meds_cycle104"] <- 1
block_trans[block_trans$initial_block == "Buprenorphine", "to_housing_nomeds_cycle104"] <- 0
block_trans[block_trans$initial_block == "Buprenorphine", "to_corresponding_post_trt_cycle104"] <- 0

block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle104"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Buprenorphine", 45:54]))


## Naltrexone

# 52
# already calibrated

# 53
block_trans[block_trans$initial_block == "Naltrexone", "to_housing_meds_cycle53"] <- 1
block_trans[block_trans$initial_block == "Naltrexone", "to_housing_nomeds_cycle53"] <- 0
block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle53"] <- 0

block_trans[block_trans$initial_block == "Naltrexone", "to_corresponding_post_trt_cycle53"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Naltrexone", 15:24]))

# 56
block_trans[block_trans$initial_block == "Naltrexone", "to_housing_meds_cycle56"] <- 1
block_trans[block_trans$initial_block == "Naltrexone", "to_housing_nomeds_cycle56"] <- 0
block_trans[block_trans$initial_block == "Naltrexone", "to_corresponding_post_trt_cycle56"] <- 0

block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle56"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Naltrexone", 25:34]))

# 65
block_trans[block_trans$initial_block == "Naltrexone", "to_housing_meds_cycle65"] <- 1
block_trans[block_trans$initial_block == "Naltrexone", "to_housing_nomeds_cycle65"] <- 0
block_trans[block_trans$initial_block == "Naltrexone", "to_corresponding_post_trt_cycle65"] <- 0

block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle65"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Naltrexone", 35:44]))

# 104
block_trans[block_trans$initial_block == "Naltrexone", "to_housing_meds_cycle104"] <- 1
block_trans[block_trans$initial_block == "Naltrexone", "to_housing_nomeds_cycle104"] <- 0
block_trans[block_trans$initial_block == "Naltrexone", "to_corresponding_post_trt_cycle104"] <- 0

block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle104"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Naltrexone", 45:54]))

## Methadone

# 52
# already calibrated

# 53
block_trans[block_trans$initial_block == "Methadone", "to_housing_meds_cycle53"] <- 1
block_trans[block_trans$initial_block == "Methadone", "to_housing_nomeds_cycle53"] <- 0
block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle53"] <- 0

block_trans[block_trans$initial_block == "Methadone", "to_corresponding_post_trt_cycle53"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Methadone", 15:24]))

# 56
block_trans[block_trans$initial_block == "Methadone", "to_housing_meds_cycle56"] <- 1
block_trans[block_trans$initial_block == "Methadone", "to_housing_nomeds_cycle53"] <- 0
block_trans[block_trans$initial_block == "Methadone", "to_corresponding_post_trt_cycle56"] <- 0

block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle56"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Methadone", 25:34]))

# 65
block_trans[block_trans$initial_block == "Methadone", "to_housing_meds_cycle65"] <- 1
block_trans[block_trans$initial_block == "Methadone", "to_housing_nomeds_cycle65"] <- 0
block_trans[block_trans$initial_block == "Methadone", "to_corresponding_post_trt_cycle65"] <- 0

block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle65"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Methadone", 35:44]))

# 104
block_trans[block_trans$initial_block == "Methadone", "to_housing_meds_cycle104"] <- 1
block_trans[block_trans$initial_block == "Methadone", "to_housing_nomeds_cycle104"] <- 0
block_trans[block_trans$initial_block == "Methadone", "to_corresponding_post_trt_cycle104"] <- 0

block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle104"] <- 0

(rowSums(block_trans[block_trans$initial_block == "Methadone", 45:54]))

## Corrections

# 52
block_trans[block_trans$initial_block == "Corrections", "to_Corrections_cycle52"] <- 0.85
block_trans[block_trans$initial_block == "Corrections", "to_corresponding_post_trt_cycle52"] <- 0.15

# 53
block_trans[block_trans$initial_block == "Corrections", "to_Corrections_cycle53"] <- 0.85
block_trans[block_trans$initial_block == "Corrections", "to_corresponding_post_trt_cycle53"] <- 0.15

# 56
block_trans[block_trans$initial_block == "Corrections", "to_Corrections_cycle56"] <- 0.85
block_trans[block_trans$initial_block == "Corrections", "to_corresponding_post_trt_cycle56"] <- 0.15

# 65
block_trans[block_trans$initial_block == "Corrections", "to_Corrections_cycle65"] <- 0.85
block_trans[block_trans$initial_block == "Corrections", "to_corresponding_post_trt_cycle65"] <- 0.15

# 104
block_trans[block_trans$initial_block == "Corrections", "to_Corrections_cycle104"] <- 0.85
block_trans[block_trans$initial_block == "Corrections", "to_corresponding_post_trt_cycle104"] <- 0.15

# section

# 52: safeguard to keep people out of section before the sweep
block_trans[block_trans$initial_block == "section", "to_section_cycle52"] <- 0
block_trans[block_trans$initial_block == "section", "to_No_Treatment_cycle52"] <- 1

# 53: sweep week
block_trans[block_trans$initial_block == "section", "to_No_Treatment_cycle53"] <- 1

# 56
block_trans[block_trans$initial_block == "section", "to_corresponding_post_trt_cycle56"] <- 0
block_trans[block_trans$initial_block == "section", "to_section_cycle56"] <- 0
block_trans[block_trans$initial_block == "section", "to_No_Treatment_cycle56"] <- 1
block_trans[block_trans$initial_block == "section", "to_Buprenorphine_cycle56"] <- 0
block_trans[block_trans$initial_block == "section", "to_Methadone_cycle56"] <- 0
block_trans[block_trans$initial_block == "section", "to_Naltrexone_cycle56"] <- 0

# 65
block_trans[block_trans$initial_block == "section", "to_corresponding_post_trt_cycle65"] <- 0
block_trans[block_trans$initial_block == "section", "to_section_cycle65"] <- 0
block_trans[block_trans$initial_block == "section", "to_No_Treatment_cycle65"] <- 1
block_trans[block_trans$initial_block == "section", "to_Buprenorphine_cycle65"] <- 0
block_trans[block_trans$initial_block == "section", "to_Methadone_cycle65"] <- 0
block_trans[block_trans$initial_block == "section", "to_Naltrexone_cycle65"] <- 0

# 104
# 90 days max for section, so kick everyone out here (starting wk 66)
block_trans[block_trans$initial_block == "section", "to_corresponding_post_trt_cycle104"] <- 0
block_trans[block_trans$initial_block == "section", "to_section_cycle104"] <- 0
block_trans[block_trans$initial_block == "section", "to_No_Treatment_cycle104"] <- 1
block_trans[block_trans$initial_block == "section", "to_Buprenorphine_cycle104"] <- 0
block_trans[block_trans$initial_block == "section", "to_Methadone_cycle104"] <- 0
block_trans[block_trans$initial_block == "section", "to_Naltrexone_cycle104"] <- 0

## diaspora

# 52
# safeguard to keep everyone out of diaspora before sweep week
block_trans[block_trans$initial_block == "diaspora", "to_diaspora_cycle52"] <- 0
block_trans[block_trans$initial_block == "diaspora", "to_No_Treatment_cycle52"] <- 1

# 53: sweep week
block_trans[block_trans$initial_block == "diaspora", "to_diaspora_cycle53"] <- 1

# 56: 3 weeks post-sweep
block_trans[block_trans$initial_block == "diaspora", "to_corresponding_post_trt_cycle56"] <- 1
block_trans[block_trans$initial_block == "diaspora", "to_No_Treatment_cycle56"] <- 0

# 65: 3 months post-sweep
block_trans[block_trans$initial_block == "diaspora", "to_corresponding_post_trt_cycle65"] <- 1
block_trans[block_trans$initial_block == "diaspora", "to_No_Treatment_cycle65"] <- 0

# 104: 9 months post-sweep
block_trans[block_trans$initial_block == "diaspora", "to_corresponding_post_trt_cycle104"] <- 1
block_trans[block_trans$initial_block == "diaspora", "to_No_Treatment_cycle104"] <- 0


## housing_meds
# use MOUD rates to transition people out of meds + housing
# 52: safeguard to keep people out
block_trans[block_trans$initial_block == "housing_meds", "to_housing_meds_cycle52"] <- 0
block_trans[block_trans$initial_block == "housing_meds", "to_No_Treatment_cycle52"] <- 1

# 53: housing week
block_trans[block_trans$initial_block == "housing_meds", "to_housing_meds_cycle53"] <- 1

# 56
block_trans[block_trans$initial_block == "housing_meds", "to_corresponding_post_trt_cycle56"] <- 0.000911
block_trans[block_trans$initial_block == "housing_meds", "to_housing_meds_cycle56"] <- 
  1 - block_trans[block_trans$initial_block == "housing_meds", "to_corresponding_post_trt_cycle56"]

# 65: 3 months post
block_trans[block_trans$initial_block == "housing_meds", "to_corresponding_post_trt_cycle65"] <- 0.000911
block_trans[block_trans$initial_block == "housing_meds", "to_housing_meds_cycle65"] <- 
  1 - block_trans[block_trans$initial_block == "housing_meds", "to_corresponding_post_trt_cycle65"]

# 104: 9 months post
block_trans[block_trans$initial_block == "housing_meds", "to_corresponding_post_trt_cycle104"] <- 0.000911
block_trans[block_trans$initial_block == "housing_meds", "to_housing_meds_cycle104"] <- 
  1 - block_trans[block_trans$initial_block == "housing_meds", "to_corresponding_post_trt_cycle104"]

## housing_nomeds
# skip post-state, go right back to No Treatment

# 52: safeguard to keep people out
block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_nomeds_cycle52"] <- 0
block_trans[block_trans$initial_block == "housing_nomeds", "to_No_Treatment_cycle52"] <- 1

# 53: housing week
block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_nomeds_cycle53"] <- 1

# 56
block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_meds_cycle56"] <- 0.00551707229
  # math from Jess Taylor's data that 25% of individuals in housing took up mmt within one year

block_trans[block_trans$initial_block == "housing_nomeds", "to_No_Treatment_cycle56"] <- 0.000911

block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_nomeds_cycle56"] <- 
  (1 - block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_meds_cycle56"]
   - block_trans[block_trans$initial_block == "housing_nomeds", "to_No_Treatment_cycle56"])

# 65: 3 months post
block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_meds_cycle65"] <- 0.00551707229

block_trans[block_trans$initial_block == "housing_nomeds", "to_No_Treatment_cycle65"] <- 0.000911

block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_nomeds_cycle65"] <- 
  (1 - block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_meds_cycle65"]
   - block_trans[block_trans$initial_block == "housing_nomeds", "to_No_Treatment_cycle65"])

# 104: 9 months post
block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_meds_cycle104"] <- 0.00551707229

block_trans[block_trans$initial_block == "housing_nomeds", "to_No_Treatment_cycle104"] <- 0.000911

block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_nomeds_cycle104"] <- 
  (1 - block_trans[block_trans$initial_block == "housing_nomeds", "to_housing_meds_cycle104"]
   - block_trans[block_trans$initial_block == "housing_nomeds", "to_No_Treatment_cycle104"])

## post-blocks

# post-housing_nomeds not included in the loop because people don't go there
block_trans[block_trans$initial_block == "post-housing_nomeds", "to_No_Treatment_cycle52"] <- 1
block_trans[block_trans$initial_block == "post-housing_nomeds", "to_No_Treatment_cycle53"] <- 1
block_trans[block_trans$initial_block == "post-housing_nomeds", "to_No_Treatment_cycle56"] <- 1
block_trans[block_trans$initial_block == "post-housing_nomeds", "to_No_Treatment_cycle65"] <- 1
block_trans[block_trans$initial_block == "post-housing_nomeds", "to_No_Treatment_cycle104"] <- 1

## post blocks are easy
post_blocks <- list("post-housing_meds", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "Post-Buprenorphine", "post-section", "post-diaspora")

# iterate through cycles by hand for now
post_cycle <- 52

for (blocks in post_blocks) {
  block_trans[block_trans$initial_block == blocks, paste("to_corresponding_post_trt_cycle", post_cycle, sep = "")] <- .75
  block_trans[block_trans$initial_block == blocks, paste("to_No_Treatment_cycle", post_cycle, sep = "")] <- .25
}

post_cycle <- 53

for (blocks in post_blocks) {
  block_trans[block_trans$initial_block == blocks, paste("to_corresponding_post_trt_cycle", post_cycle, sep = "")] <- .75
  block_trans[block_trans$initial_block == blocks, paste("to_No_Treatment_cycle", post_cycle, sep = "")] <- .25
}

post_cycle <- 56

for (blocks in post_blocks) {
  block_trans[block_trans$initial_block == blocks, paste("to_corresponding_post_trt_cycle", post_cycle, sep = "")] <- .75
  block_trans[block_trans$initial_block == blocks, paste("to_No_Treatment_cycle", post_cycle, sep = "")] <- .25
}

post_cycle <- 65

for (blocks in post_blocks) {
  block_trans[block_trans$initial_block == blocks, paste("to_corresponding_post_trt_cycle", post_cycle, sep = "")] <- .75
  block_trans[block_trans$initial_block == blocks, paste("to_No_Treatment_cycle", post_cycle, sep = "")] <- .25
}

post_cycle <- 104

for (blocks in post_blocks) {
  block_trans[block_trans$initial_block == blocks, paste("to_corresponding_post_trt_cycle", post_cycle, sep = "")] <- .75
  block_trans[block_trans$initial_block == blocks, paste("to_No_Treatment_cycle", post_cycle, sep = "")] <- .25
}

# select out cycle 156 columns
block_trans <- block_trans %>%
  select(-to_No_Treatment_cycle156, -to_Buprenorphine_cycle156, -to_Naltrexone_cycle156, -to_Methadone_cycle156, -to_corresponding_post_trt_cycle156)

write.csv(block_trans, paste0("input", input_number, "/block_trans.csv"), row.names = F)

### entering cohort ##########

enter_cohort <- read.csv(paste0("input", input_number, "/entering_cohort.csv"))
enter_cohort[, c(4)] <- rep(0, length.out = 90)

enter_cohort <- enter_cohort %>%
  select(agegrp, sex, number_of_new_comers_cycle104)

enter_cohort <- enter_cohort %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(enter_cohort, paste0("input", input_number, "/entering_cohort.csv"), row.names = F)

### fatal od ##########

fatal_od <- read.csv(paste0("input", input_number, "/fatal_overdose.csv"))

# switch to rate, multiply, then switch back
updated_fatal_od <- 
  -ln(1 - fatal_od$fatal_to_all_types_overdose_ratio_cycle156)
updated_fatal_od <- updated_fatal_od * 2.3
updated_fatal_od <- 1 - (exp(-updated_fatal_od))

fatal_od[c(1), c(1)] <- updated_fatal_od
fatal_od[c(1), c(2)] <- updated_fatal_od

fatal_od <- fatal_od %>%
  mutate(fatal_to_all_types_overdose_ratio_cycle53 = fatal_to_all_types_overdose_ratio_cycle52) %>%
  mutate(fatal_to_all_types_overdose_ratio_cycle56 = fatal_to_all_types_overdose_ratio_cycle52) %>%
  mutate(fatal_to_all_types_overdose_ratio_cycle65 = fatal_to_all_types_overdose_ratio_cycle52)

fatal_od <- fatal_od %>%
  select(fatal_to_all_types_overdose_ratio_cycle52, fatal_to_all_types_overdose_ratio_cycle53,
         fatal_to_all_types_overdose_ratio_cycle56, fatal_to_all_types_overdose_ratio_cycle65,
         fatal_to_all_types_overdose_ratio_cycle104)

write.csv(fatal_od, paste0("input", input_number, "/fatal_overdose.csv"), row.names = F)

### oud trans ##########
# must sum to 1

oud_trans <- read.csv(paste0("input", input_number, "/oud_trans.csv"))

# remove Detox and Post detox
oud_trans <- oud_trans %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox")

# add empty rows to oud_trans (base)
corr_ot <- oud_trans %>%
  filter(block == "No_Treatment")
corr_ot[, c(5:8)] <- rep(0, length.out = 360)
corr_ot$block <- "Corrections"

p_corr_ot <- oud_trans %>%
  filter(block == "No_Treatment")
p_corr_ot[, c(5:8)] <- rep(0, length.out = 360)
p_corr_ot$block <- "Post-Corrections"

sect_ot <- oud_trans %>%
  filter(block == "No_Treatment")
sect_ot[, c(5:8)] <- rep(0, length.out = 360)
sect_ot$block <- "section"

p_sect_ot <- oud_trans %>%
  filter(block == "No_Treatment")
p_sect_ot[, c(5:8)] <- rep(0, length.out = 360)
p_sect_ot$block <- "post-section"

diasp_ot <- oud_trans %>%
  filter(block == "No_Treatment")
diasp_ot[, c(5:8)] <- rep(0, length.out = 360)
diasp_ot$block <- "diaspora"

p_diasp_ot <- oud_trans %>%
  filter(block == "No_Treatment")
p_diasp_ot[, c(5:8)] <- rep(0, length.out = 360)
p_diasp_ot$block <- "post-diaspora"

housing_ot <- oud_trans %>%
  filter(block == "No_Treatment")
housing_ot[, c(5:8)] <- rep(0, length.out = 360)
housing_ot$block <- "housing_meds"

p_housing_ot <- oud_trans %>%
  filter(block == "No_Treatment")
p_housing_ot[, c(5:8)] <- rep(0, length.out = 360)
p_housing_ot$block <- "post-housing_meds"

housing_nm_ot <- oud_trans %>%
  filter(block == "No_Treatment")
housing_nm_ot[, c(5:8)] <- rep(0, length.out = 360)
housing_nm_ot$block <- "housing_nomeds"

p_housing_nm_ot <- oud_trans %>%
  filter(block == "No_Treatment")
p_housing_nm_ot[, c(5:8)] <- rep(0, length.out = 360)
p_housing_nm_ot$block <- "post-housing_nomeds"

oud_trans <- rbind(oud_trans, corr_ot, p_corr_ot, sect_ot, p_sect_ot, diasp_ot, p_diasp_ot, 
                   housing_ot, p_housing_ot, housing_nm_ot, p_housing_nm_ot)

# corrections = no treatment
oud_trans[oud_trans$block == "Corrections", 5:8] <- oud_trans[oud_trans$block == "No_Treatment", 5:8]

# section = detox
oud_trans[oud_trans$block == "section" & oud_trans$initial_status == "Active_Noninjection", "to_Active_Noninjection"] <- 1
oud_trans[oud_trans$block == "section" & oud_trans$initial_status == "Active_Injection", "to_Active_Injection"] <- 1
oud_trans[oud_trans$block == "section" & oud_trans$initial_status == "Nonactive_Noninjection", "to_Nonactive_Noninjection"] <- 1
oud_trans[oud_trans$block == "section" & oud_trans$initial_status == "Nonactive_Injection", "to_Nonactive_Injection"] <- 1

# diaspora
oud_trans[oud_trans$block == "diaspora", 5:8] <- oud_trans[oud_trans$block == "No_Treatment", 5:8]

# housing_meds = methadone
oud_trans[oud_trans$block == "housing_meds", 5:8] <- oud_trans[oud_trans$block == "Methadone", 5:8]

# housing_nomeds = no treatment
oud_trans[oud_trans$block == "housing_nomeds", 5:8] <- oud_trans[oud_trans$block == "No_Treatment", 5:8]

# post-Corrections = post-bup
oud_trans[oud_trans$block == "Post-Corrections", 5:8] <- oud_trans[oud_trans$block == "Post-Buprenorphine", 5:8]

# post-section = post-bup
oud_trans[oud_trans$block == "post-section", 5:8] <- oud_trans[oud_trans$block == "Post-Buprenorphine", 5:8]

# post diaspora = no treatment
oud_trans[oud_trans$block == "post-diaspora", 5:8] <- oud_trans[oud_trans$block == "No_Treatment", 5:8]

# post-housing_meds = post-mmt
oud_trans[oud_trans$block == "post-housing_meds", 5:8] <- oud_trans[oud_trans$block == "Post-Methadone", 5:8]

# post-housing_nomeds = no treatment
oud_trans[oud_trans$block == "post-housing_nomeds", 5:8] <- oud_trans[oud_trans$block == "No_Treatment", 5:8]

problems <- subset(oud_trans, rowSums((oud_trans[, c(5:8)])) != 1)
problems <- problems %>% mutate(sums = rowSums(problems[, 5:8]))

oud_trans <- oud_trans %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section",
      "diaspora", "housing_meds", "housing_nomeds", "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", 
      "Post-Corrections", "post-section", "post-diaspora", "post-housing_meds", "post-housing_nomeds"
    )),
    agegrp, desc(sex), factor(initial_status, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

oud_trans <- oud_trans %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(oud_trans, paste0("input", input_number, "/oud_trans.csv"), row.names = F)

### SMR ##########

SMR <- read.csv(paste0("input", input_number, "/SMR.csv"))

# add empty rows to SMR (base)
corr_smr <- SMR %>%
  filter(block == "No_Treatment")
corr_smr[, c(5)] <- rep(0, length.out = 360)
corr_smr$block <- "Corrections"

p_corr_smr <- SMR %>%
  filter(block == "No_Treatment")
p_corr_smr[, c(5)] <- rep(0, length.out = 360)
p_corr_smr$block <- "Post-Corrections"

sect_smr <- SMR %>%
  filter(block == "No_Treatment")
sect_smr[, c(5)] <- rep(0, length.out = 360)
sect_smr$block <- "section"

p_sect_smr <- SMR %>%
  filter(block == "No_Treatment")
p_sect_smr[, c(5)] <- rep(0, length.out = 360)
p_sect_smr$block <- "post-section"

diasp_smr <- SMR %>%
  filter(block == "No_Treatment")
diasp_smr[, c(5)] <- rep(0, length.out = 360)
diasp_smr$block <- "diaspora"

p_diasp_smr <- SMR %>%
  filter(block == "No_Treatment")
p_diasp_smr[, c(5)] <- rep(0, length.out = 360)
p_diasp_smr$block <- "post-diaspora"

housing_smr <- SMR %>%
  filter(block == "No_Treatment")
housing_smr[, c(5)] <- rep(0, length.out = 360)
housing_smr$block <- "housing_meds"

p_housing_smr <- SMR %>%
  filter(block == "No_Treatment")
p_housing_smr[, c(5)] <- rep(0, length.out = 360)
p_housing_smr$block <- "post-housing_meds"

housing_nm_smr <- SMR %>%
  filter(block == "No_Treatment")
housing_nm_smr[, c(5)] <- rep(0, length.out = 360)
housing_nm_smr$block <- "housing_nomeds"

p_housing_nm_smr <- SMR %>%
  filter(block == "No_Treatment")
p_housing_nm_smr[, c(5)] <- rep(0, length.out = 360)
p_housing_nm_smr$block <- "post-housing_nomeds"

SMR <- rbind(SMR, corr_smr, p_corr_smr, sect_smr, p_sect_smr, diasp_smr, 
             p_diasp_smr, housing_smr, p_housing_smr, housing_nm_smr,
             p_housing_nm_smr)

# Corrections
SMR[SMR$block == "Corrections", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# section
SMR[SMR$block == "section", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# diaspora
SMR[SMR$block == "diaspora", "SMR"] <- SMR %>%
  filter(block == "Post-Buprenorphine") %>%
  select(SMR)

# housing_meds = methadone
SMR[SMR$block == "housing_meds", "SMR"] <- SMR %>%
  filter(block == "Methadone") %>%
  select(SMR)

# housing_nomeds = no treatment
SMR[SMR$block == "housing_nomeds", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# post-corrections
SMR[SMR$block == "Post-Corrections", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# post-section
SMR[SMR$block == "post-section", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# post-diaspora
SMR[SMR$block == "post-diaspora", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# post-housing_meds = post-mmt
SMR[SMR$block == "post-housing_meds", "SMR"] <- SMR %>%
  filter(block == "Post-Methadone") %>%
  select(SMR)

# post-housing_nomeds = no treatment, will be skipped
SMR[SMR$block == "post-housing_nomeds", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# remove Detox and post-detox
SMR <- SMR %>% filter(block != "Detox")
SMR <- SMR %>% filter(block != "Post-Detox")

# percent change: bringing highest value (5.671) to 9.8, all others up equivalently
# 9.8 is overall SMR
# remove fatal od (24.2%)
9.8 * (1 - 0.242)
(7.428 - 4.66) / 4.66
4.66 * 1.594

SMR$SMR <- SMR$SMR * (1.594)

# arrange

SMR <- SMR %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17") %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora", "housing_meds",
      "housing_nomeds", "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections",
      "post-section", "post-diaspora", "post-housing_meds", "post-housing_nomeds"
    )),
    agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

write.csv(SMR, paste0("input", input_number, "/SMR.csv"), row.names = F)

###### UTILITIES #############################################################

## utility adjustments ##
nonhousing <- -0.0502
diaspora <- -0.02378

### background utility ###########

bg_utility <- read.csv(paste0("input", input_number, "/cost_life/bg_utility.csv"))

bg_utility$utility <- bg_utility$utility + nonhousing
  # adjust for not being housed

bg_utility <- bg_utility %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(bg_utility, paste0("input", input_number, "/cost_life/bg_utility.csv"), row.names = F)

### oud utility ##############

oud_utility <- read.csv(paste0("input", input_number, "/cost_life/oud_utility.csv"))

corr_out <- oud_utility %>%
  filter(block == "No_Treatment")
corr_out$block <- "Corrections"
corr_out$utility <- corr_out$utility + nonhousing

p_corr_out <- oud_utility %>%
  filter(block == "Post-Buprenorphine")
p_corr_out$block <- "Post-Corrections"
p_corr_out$utility <- p_corr_out$utility + nonhousing

sect_out <- oud_utility %>%
  filter(block == "No_Treatment")
sect_out$block <- "section"

p_sect_out <- oud_utility %>%
  filter(block == "No_Treatment")
p_sect_out$block <- "post-section"
p_sect_out$utility <- p_sect_out$utility + nonhousing

diasp_out <- oud_utility %>%
  filter(block == "No_Treatment")
diasp_out$block <- "diaspora"
diasp_out$utility <- diasp_out$utility + nonhousing + diaspora

p_diasp_out <- oud_utility %>%
  filter(block == "No_Treatment")
p_diasp_out$block <- "post-diaspora"
p_diasp_out$utility <- p_diasp_out$utility + nonhousing

housing_out <- oud_utility %>%
  filter(block == "Methadone")
housing_out$block <- "housing_meds"

p_housing_out <- oud_utility %>%
  filter(block == "Post-Methadone")
p_housing_out$block <- "post-housing_meds"
p_housing_out$utility <- p_housing_out$utility + nonhousing

housing_nm_out <- oud_utility %>%
  filter(block == "No_Treatment")
housing_nm_out$block <- "housing_nomeds"

p_housing_nm_out <- oud_utility %>%
  filter(block == "No_Treatment")
p_housing_nm_out$block <- "post-housing_nomeds"
p_housing_nm_out$utility <- p_housing_nm_out$utility + nonhousing

oud_utility <- oud_utility %>%
  rbind(corr_out, p_corr_out, sect_out, p_sect_out, diasp_out, p_diasp_out,
        housing_out, p_housing_out, housing_nm_out, p_housing_nm_out)

# housing adjustment
oud_utility[oud_utility$block == "No_Treatment", "utility"] <- 
  oud_utility[oud_utility$block == "No_Treatment", "utility"] + nonhousing

oud_utility[oud_utility$block == "Buprenorphine", "utility"] <- 
  oud_utility[oud_utility$block == "Buprenorphine", "utility"] + nonhousing

oud_utility[oud_utility$block == "Naltrexone", "utility"] <- 
  oud_utility[oud_utility$block == "Naltrexone", "utility"] + nonhousing

oud_utility[oud_utility$block == "Methadone", "utility"] <- 
  oud_utility[oud_utility$block == "Methadone", "utility"] + nonhousing

oud_utility[oud_utility$block == "Post-Buprenorphine", "utility"] <- 
  oud_utility[oud_utility$block == "Post-Buprenorphine", "utility"] + nonhousing

oud_utility[oud_utility$block == "Post-Naltrexone", "utility"] <- 
  oud_utility[oud_utility$block == "Post-Naltrexone", "utility"] + nonhousing

oud_utility[oud_utility$block == "Post-Methadone", "utility"] <- 
  oud_utility[oud_utility$block == "Post-Methadone", "utility"] + nonhousing

oud_utility <- oud_utility %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox") %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora", "housing_meds",
      "housing_nomeds", "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "post-section",
      "post-diaspora", "post-housing_meds", "post-housing_nomeds")),
    factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

write.csv(oud_utility, paste0("input", input_number, "/cost_life/oud_utility.csv"), row.names = F)

### setting utility ###########

setting_utility <- read.csv(paste0("input", input_number, "/cost_life/setting_utility.csv"))

# add housing/post
# UPDATE UTILITY HERE
setting_utility <- setting_utility %>%
  add_row(block = "Corrections",
          utility = 0.711 + nonhousing) %>%
  add_row(block = "Post-Corrections",
          utility = 1) %>%
  add_row(block = "section",
          utility = 0.78) %>%
  add_row(block = "post-section",
          utility = 1) %>%
  add_row(block = "diaspora",
          utility = 1) %>%
  add_row(block = "post-diaspora",
          utility = 1) %>%
  add_row(block = "housing_meds",
          utility = 1) %>%
  add_row(block = "post-housing_meds",
          utility = 1) %>%
  add_row(block = "housing_nomeds",
          utility = 1) %>%
  add_row(block = "post-housing_nomeds",
          utility = 1)

# re-arrange
setting_utility <- setting_utility %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox") %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora", "housing_meds", "housing_nomeds",
      "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "post-section", "post-diaspora", "post-housing_meds", "post-housing_nomeds"))
  )

write.csv(setting_utility, paste0("input", input_number, "/cost_life/setting_utility.csv"), row.names = F)

##### COSTS ################################################################

### overdose cost ############

overdose_cost <- read.csv(paste0("input", input_number, "/cost_life/overdose_cost.csv"))
# it's already correct, don't change anything.
write.csv(overdose_cost, paste0("input", input_number, "/cost_life/overdose_cost.csv"), row.names = F)

### pharmaceutical cost #############

pharmaceutical_cost <- read.csv(paste0("input", input_number, "/cost_life/pharmaceutical_cost.csv"))

pharmaceutical_cost <- pharmaceutical_cost %>%
  filter(block != "Detox") %>%
  add_row(block = "Corrections",
          pharmaceutical_cost_.healthcare_system = 0) %>%
  add_row(block = "section",
          pharmaceutical_cost_.healthcare_system = 0) %>%
  add_row(block = "diaspora",
          pharmaceutical_cost_.healthcare_system = 0) %>%
  add_row(block = "housing_meds", pharmaceutical_cost_.healthcare_system = 
          pharmaceutical_cost[pharmaceutical_cost$block == "Methadone", "pharmaceutical_cost_.healthcare_system"]) %>%
  add_row(block = "housing_nomeds", 
          pharmaceutical_cost_.healthcare_system = 0)
  
write.csv(pharmaceutical_cost, paste0("input", input_number, "/cost_life/pharmaceutical_cost.csv"), row.names = F)

### treatment utilization cost ##############
# ADD HOUSING COST HERE

treatment_utilization_cost <- read.csv(paste0("input", input_number, "/cost_life/treatment_utilization_cost.csv"))

treatment_utilization_cost <- treatment_utilization_cost %>%
  add_row(block = "Corrections",
          treatment_utilization_cost_.healthcare_system = 0) %>%
  add_row(block = "section",
          treatment_utilization_cost_.healthcare_system = 1477.23) %>%
  add_row(block = "diaspora",
          treatment_utilization_cost_.healthcare_system = 0) %>%
  add_row(block = "housing_meds",
          treatment_utilization_cost_.healthcare_system = 416.34) %>% # housing plus methadone
  add_row(block = "housing_nomeds",
          treatment_utilization_cost_.healthcare_system = 292.91) %>% # housing
  filter(block != "Detox")
  
write.csv(treatment_utilization_cost, paste0("input", input_number, "/cost_life/treatment_utilization_cost.csv"), row.names = F)

### healthcare utilization ##########
# UPDATE TO LOWER HC COSTS

healthcare_utilization_cost <- read.csv(paste0("input", input_number, "/cost_life/healthcare_utilization_cost.csv"))

# add new blocks
corr_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
corr_huc$block <- "Corrections"
corr_huc[, c(5)] <- rep(840, length.out = 4)

p_corr_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
p_corr_huc$block <- "Post-Corrections"

sect_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
sect_huc$block <- "section"
sect_huc[, c(5)] <- rep(0, length.out = 4)

p_sect_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
p_sect_huc$block <- "post-section"

diasp_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
diasp_huc$block <- "diaspora"
diasp_huc[, c(5)] <- rep(0, length.out = 4)

p_diasp_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
p_diasp_huc$block <- "post-diaspora"

housing_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment") %>%
  mutate(new_cost = healthcare_utilization_cost_healthcare_system*0.48488) %>%
  select(-healthcare_utilization_cost_healthcare_system) %>%
  rename(healthcare_utilization_cost_healthcare_system = new_cost)
# housing_huc$healthcare_utilization_cost_healthcare_system <- housing_huc$healthcare_utilization_cost_healthcare_system*0.48488
housing_huc$block <- "housing_meds"

p_housing_huc <- healthcare_utilization_cost %>%
  filter(block == "Post-Methadone")
p_housing_huc$block <- "post-housing_meds"

housing_nm_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment") %>%
  mutate(new_cost = healthcare_utilization_cost_healthcare_system*0.48488) %>%
  select(-healthcare_utilization_cost_healthcare_system) %>%
  rename(healthcare_utilization_cost_healthcare_system = new_cost)
# housing_huc$healthcare_utilization_cost_healthcare_system <- housing_huc$healthcare_utilization_cost_healthcare_system*0.48488
housing_nm_huc$block <- "housing_nomeds"

p_housing_nm_huc <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
p_housing_nm_huc$block <- "post-housing_nomeds"

healthcare_utilization_cost <- healthcare_utilization_cost %>%
  rbind(corr_huc, p_corr_huc, sect_huc, p_sect_huc, diasp_huc, p_diasp_huc,
        housing_huc, p_housing_huc, housing_nm_huc, p_housing_nm_huc)

healthcare_utilization_cost <- healthcare_utilization_cost %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox") %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora", "housing_meds", "housing_nomeds",
      "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "post-section", "post-diaspora", "post-housing_meds", "post-housing_nomeds")),
    agegrp, desc(sex), 
    factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

healthcare_utilization_cost <- healthcare_utilization_cost %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(healthcare_utilization_cost, paste0("input", input_number, "/cost_life/healthcare_utilization_cost.csv"), row.names = F)

##### READY TO RUN MODEL ####################################################

