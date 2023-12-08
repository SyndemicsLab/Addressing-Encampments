# Mass Cass Sweeps Project

## Script for Sweep scenario
## Author: Hana Zwick
##  Last updated 09/01/2023

# This combines code from new_mass_cass_inputs_good.R and EC_MC script.R because
# the inputs weren't working
# ie. new_mass_cass_inputs_good.R wanted to use init_cohort_scratch.csv to fill
# in values but that CSV only has no treatment and bup, no other blocks. EC_MC
# script uses EC_basecase to fill in the inputs. EC_MC script is tailorable to
# all three scenarios, but I'd like three working scripts so I don't have to
# change the code every time I run a different scenario.

getwd()

## libraries
library(dplyr)
library(SciViews)

## set variables
arguments <- commandArgs(trailingOnly = TRUE)
input_number <- arguments[1]


##### PROBABILITIES ########################################################

### initial cohort ##########

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  init_cohort <- read.csv(paste0(input_folder,'/',"init_cohort.csv"))

init_cohort <- init_cohort %>%
  filter(block != "Detox") %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17") %>%
  arrange(
    factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora")),
    agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

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

init_cohort <- init_cohort %>%
  rbind(new_corr, new_section, new_diasp)

write.csv(init_cohort, paste0(input_folder,'/',"init_cohort.csv"), row.names = F)

}

### all types OD ###########

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  all_types_od <- read.csv(paste0(input_folder,'/',"all_types_overdose.csv"))
  
# add new columns, re-write so all are equivalent to cycle 156
all_types_od <- all_types_od %>%
  mutate(all_types_overdose_cycle52 = all_types_overdose_cycle156) %>%
  mutate(all_types_overdose_cycle53 = all_types_overdose_cycle156) %>%
  mutate(all_types_overdose_cycle56 = all_types_overdose_cycle156) %>%
  mutate(all_types_overdose_cycle65 = all_types_overdose_cycle156) %>%
  mutate(all_types_overdose_cycle104 = all_types_overdose_cycle156)

# re-arrange columns, remove cycle 156
all_types_od <- all_types_od %>%
  select(block, agegrp, sex, oud,
         all_types_overdose_cycle52, all_types_overdose_cycle53, all_types_overdose_cycle56,
         all_types_overdose_cycle65, all_types_overdose_cycle104)
  # should have 9 columns

# remove rows (age)
all_types_od <- all_types_od %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

# remove Detox/Post-Detox (rows)
all_types_od <- all_types_od %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox")

# add Corrections/section/diaspora/post-blocks (rows)
new_block1 <- all_types_od %>%
  filter(block == "No_Treatment")
new_block1$block <- "Corrections"

new_block2 <- all_types_od %>%
  filter(block == "No_Treatment")
new_block2$block <- "section"

new_block3 <- all_types_od %>%
  filter(block == "No_Treatment")
new_block3$block <- "diaspora"

new_block4 <- all_types_od %>%
  filter(block == "Post-Buprenorphine")
new_block4$block <- "Post-Corrections"

new_block5 <- all_types_od %>%
  filter(block == "Post-Buprenorphine")
new_block5$block <- "post-section"

new_block6 <- all_types_od %>%
  filter(block == "No_Treatment")
new_block6$block <- "post-diaspora"

all_types_od <- rbind(all_types_od, new_block1, new_block2, new_block3, new_block4, new_block5, new_block6)

# re-arrange rows
all_types_od <- all_types_od %>%
  arrange(
    factor(block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora",
                             "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "post-section", "post-diaspora")),
    agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection")))
  
  # should have 104 rows

## No Treatment, bup, ntx, and mmt are already calibrated

## Corrections = No Treatment/2
all_types_od[all_types_od$block == "Corrections", "all_types_overdose_cycle52"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"] * 0.5

all_types_od[all_types_od$block == "Corrections", "all_types_overdose_cycle53"] <-
  all_types_od[all_types_od$block == "No_Treatment", "all_types_overdose_cycle65"] * 0.5

all_types_od[all_types_od$block == "Corrections", "all_types_overdose_cycle56"] <-
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
# done when adding the block

## Post-Corrections = Post-Bup
# done when adding the block

## post-section = Post-Detox (same as Post-Bup)
# done when adding the block

## post-diaspora = No Treatment
# done when adding the block

write.csv(all_types_od, paste0(input_folder,'/',"all_types_overdose.csv", sep = ""), row.names = F)

}

### background mortality #############

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  background_mort <- read.csv(paste0(input_folder,'/',"background_mortality.csv"))
# don't change - this is the denominator of SMR (general pop)

background_mort <- background_mort %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(background_mort, paste0(input_folder,'/',"background_mortality.csv", sep = ""), row.names = F)

}

### block initiation effect ##############

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  block_init_effect <- read.csv(paste0(input_folder,'/',"block_init_effect.csv"))
  
block_init_effect <- block_init_effect %>% 
  mutate(
    to_section = 1, to_diaspora = 1, to_Corrections = 1,
    to_post.section = 1, to_post.diaspora = 1, to_Post.Corrections = 1
  )
# post corrections
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Noninjection", "to_Post.Corrections"] <- 0.4
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Injection", "to_Post.Corrections"] <- 0.4

# post section
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Noninjection", "to_post.section"] <- 0.4
block_init_effect[block_init_effect$initial_oud_state == "Nonactive_Injection", "to_post.section"] <- 0.4

# clean
block_init_effect <- block_init_effect %>% select(-to_Detox, -to_Post.Detox)

# should be c,s,d
block_init_effect <- block_init_effect %>% relocate(to_Corrections, .after = to_Methadone)
block_init_effect <- block_init_effect %>% relocate(to_section, .after = to_Corrections)
block_init_effect <- block_init_effect %>% relocate(to_diaspora, .after = to_section)

block_init_effect <- block_init_effect %>% relocate(to_Post.Corrections, .after = to_Post.Methadone)
block_init_effect <- block_init_effect %>% relocate(to_post.section, .after = to_Post.Corrections)
block_init_effect <- block_init_effect %>% relocate(to_post.diaspora, .after = to_post.section)

write.csv(block_init_effect, paste0(input_folder,'/',"block_init_effect.csv", sep = ""), row.names = F)

}

### Block Transitions ############
# the probability of transitioning from no treatment to its corresponding post-treatment should be zero

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  block_trans <- read.csv(paste0(input_folder,'/',"block_trans.csv"))
  
# add in blocks (rows)
new_block7 <- block_trans %>%
  filter(initial_block == "No_Treatment")
new_block7[, c(5:22)] <- rep(0, length.out = 144)
new_block7$initial_block <- "diaspora"

new_block8 <- block_trans %>%
  filter(initial_block == "No_Treatment")
new_block8[, c(5:22)] <- rep(0, length.out = 144)
new_block8$initial_block <- "post-diaspora"

new_block9 <- block_trans %>%
  filter(initial_block == "No_Treatment")
new_block9[, c(5:22)] <- rep(0, length.out = 144)
new_block9$initial_block <- "section"

new_block10 <- block_trans %>%
  filter(initial_block == "No_Treatment")
new_block10[, c(5:22)] <- rep(0, length.out = 144)
new_block10$initial_block <- "post-section"

new_block11 <- block_trans %>%
  filter(initial_block == "No_Treatment")
new_block11[, c(5:22)] <- rep(0, length.out = 144)
new_block11$initial_block <- "Corrections"

new_block12 <- block_trans %>%
  filter(initial_block == "No_Treatment")
new_block12[, c(5:22)] <- rep(0, length.out = 144)
new_block12$initial_block <- "Post-Corrections"

block_trans <- block_trans %>%
  rbind(new_block7, new_block8, new_block9, new_block10, new_block11, new_block12)

# remove Detox, Post-Detox (rows)
block_trans <- block_trans %>%
  filter(initial_block != "Detox") %>%
  filter(initial_block != "Post-Detox")

# arrange to the correct row order

block_trans <- block_trans %>%
  arrange(agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection")), 
          factor(initial_block, levels = c("No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora",
                                           "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections",
                                           "post-section", "post-diaspora")))
          
# add new blocks (columns)

# 52
block_trans <- block_trans %>%
  mutate(
    to_Corrections_cycle52 = 0,
    to_section_cycle52 = 0,
    to_diaspora_cycle52 = 0
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
    to_corresponding_post_trt_cycle65 = 0
  )

# 104
block_trans <- block_trans %>%
  mutate(
    to_Corrections_cycle104 = 0,
    to_section_cycle104 = 0,
    to_diaspora_cycle104 = 0
  )

# re-arrange columns using select(), remove Detox/Post-Detox by doing so
block_trans <- block_trans %>%
  select(agegrp, sex, oud, initial_block, 
         to_No_Treatment_cycle52, to_Buprenorphine_cycle52, to_Naltrexone_cycle52, to_Methadone_cycle52, to_Corrections_cycle52, to_section_cycle52, to_diaspora_cycle52, to_corresponding_post_trt_cycle52,
         to_No_Treatment_cycle53, to_Buprenorphine_cycle53, to_Naltrexone_cycle53, to_Methadone_cycle53, to_Corrections_cycle53, to_section_cycle53, to_diaspora_cycle53, to_corresponding_post_trt_cycle53,
         to_No_Treatment_cycle56, to_Buprenorphine_cycle56, to_Naltrexone_cycle56, to_Methadone_cycle56, to_Corrections_cycle56, to_section_cycle56, to_diaspora_cycle56, to_corresponding_post_trt_cycle56,
         to_No_Treatment_cycle65, to_Buprenorphine_cycle65, to_Naltrexone_cycle65, to_Methadone_cycle65, to_Corrections_cycle65, to_section_cycle65, to_diaspora_cycle65, to_corresponding_post_trt_cycle65,
         to_No_Treatment_cycle104, to_Buprenorphine_cycle104, to_Naltrexone_cycle104, to_Methadone_cycle104, to_Corrections_cycle104, to_section_cycle104, to_diaspora_cycle104, to_corresponding_post_trt_cycle104,
         to_No_Treatment_cycle156, to_Buprenorphine_cycle156, to_Naltrexone_cycle156, to_Methadone_cycle156, to_corresponding_post_trt_cycle156)

  # should have 49 columns. will end with 44, but need to keep cycle 156 (5 
  # columns) until the end, then remove them before writing to a .csv.

# limit age
block_trans <- block_trans %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

  # should have 208 rows

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

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 5:12]))

# 53
block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle53"] <- .90
block_trans[block_trans$initial_block == "No_Treatment", "to_section_cycle53"] <- .10
block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle53"] <- 0

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 13:20]))

# 56
block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle56"] <-
  block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle56"] <-
  block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle56"] <-
  block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle56"] <-
  block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle52"]

block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle56"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"])

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 21:28]))

# 65
block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle65"] <-
  block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle65"] <-
  block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle65"] <-
  block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle65"] <-
  block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle52"]

block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle65"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"])

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 29:36]))

# 104
# no treatment -> bup, ntx, and mmt are calibrated

block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle104"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle156"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"])

(rowSums(block_trans[block_trans$initial_block == "No_Treatment", 37:44]))

## Buprenorphine

# 52
# already calibrated

# 53
block_trans[block_trans$initial_block == "Buprenorphine", "to_diaspora_cycle53"] <- 0
block_trans[block_trans$initial_block == "Buprenorphine", "to_corresponding_post_trt_cycle53"] <-
  (.75 - (1 - block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle52"]))
block_trans[block_trans$initial_block == "Buprenorphine", "to_section_cycle53"] <- .10
block_trans[block_trans$initial_block == "Buprenorphine", "to_Corrections_cycle53"] <- 0
block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle53"] <-
  (1 - block_trans[block_trans$initial_block == "Buprenorphine", "to_corresponding_post_trt_cycle53"]
   - block_trans[block_trans$initial_block == "Buprenorphine", "to_section_cycle53"])

(rowSums(block_trans[block_trans$initial_block == "Buprenorphine", 13:20]))

# 56
block_trans[block_trans$initial_block == "Buprenorphine", "to_corresponding_post_trt_cycle56"] <- (1 - block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle52"])
block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle56"] <- block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle52"]

(rowSums(block_trans[block_trans$initial_block == "Buprenorphine", 21:28]))

# 65
block_trans[block_trans$initial_block == "Buprenorphine", "to_corresponding_post_trt_cycle65"] <- (1 - block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle52"])
block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle65"] <- block_trans[block_trans$initial_block == "Buprenorphine", "to_Buprenorphine_cycle52"]

(rowSums(block_trans[block_trans$initial_block == "Buprenorphine", 29:36]))

# 104
# already calibrated

## Naltrexone

# 52
# already calibrated

# 53
block_trans[block_trans$initial_block == "Naltrexone", "to_diaspora_cycle53"] <- 0
block_trans[block_trans$initial_block == "Naltrexone", "to_corresponding_post_trt_cycle53"] <-
  (.75 - (1 - block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle156"]))
block_trans[block_trans$initial_block == "Naltrexone", "to_section_cycle53"] <- .10
block_trans[block_trans$initial_block == "Naltrexone", "to_Corrections_cycle53"] <- 0
block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle53"] <-
  (1 - block_trans[block_trans$initial_block == "Naltrexone", "to_corresponding_post_trt_cycle53"]
   - block_trans[block_trans$initial_block == "Naltrexone", "to_section_cycle53"])

(rowSums(block_trans[block_trans$initial_block == "Naltrexone", 13:20]))

# 56
block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle56"] <-
  block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle156"]
block_trans[block_trans$initial_block == "Naltrexone", "to_corresponding_post_trt_cycle56"] <-
  (1 - block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle156"])

(rowSums(block_trans[block_trans$initial_block == "Naltrexone", 21:28]))

# 65
block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle65"] <-
  block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle156"]
block_trans[block_trans$initial_block == "Naltrexone", "to_corresponding_post_trt_cycle65"] <-
  (1 - block_trans[block_trans$initial_block == "Naltrexone", "to_Naltrexone_cycle156"])

(rowSums(block_trans[block_trans$initial_block == "Naltrexone", 29:36]))

# 104
# already calibrated

## Methadone

# 52
# already calibrated

# 53
block_trans[block_trans$initial_block == "Methadone", "to_diaspora_cycle53"] <- 0
block_trans[block_trans$initial_block == "Methadone", "to_corresponding_post_trt_cycle53"] <-
  (.75 - (1 - block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle156"]))
block_trans[block_trans$initial_block == "Methadone", "to_section_cycle53"] <- .10
block_trans[block_trans$initial_block == "Methadone", "to_Corrections_cycle53"] <- 0
block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle53"] <-
  (1 - block_trans[block_trans$initial_block == "Methadone", "to_corresponding_post_trt_cycle53"]
   - block_trans[block_trans$initial_block == "Methadone", "to_section_cycle53"])

# 56
block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle56"] <-
  block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle156"]
block_trans[block_trans$initial_block == "Methadone", "to_corresponding_post_trt_cycle56"] <-
  (1 - block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle156"])

# 65
block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle65"] <-
  block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle156"]
block_trans[block_trans$initial_block == "Methadone", "to_corresponding_post_trt_cycle65"] <-
  (1 - block_trans[block_trans$initial_block == "Methadone", "to_Methadone_cycle156"])

# 104
# already calibrated

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
block_trans[block_trans$initial_block == "section", "to_section_cycle53"] <- 1

# 56
# post section = post-corrections so we don't want to have people stay in that too long
# probability of weekly exit is 0.215416759
# probability of staying is 1-0.215416759
# 7.7% of people leave section 35 with MOUD

bup_1 <- 0.00264
mmt_1 <- 0.000605
ntx_1 <- 0.000274
corres_1 <- 0.996

bup_p <- 0.00264 / (sum(bup_1, mmt_1, ntx_1, corres_1))
mmt_p <- 0.000605 / (sum(bup_1, mmt_1, ntx_1, corres_1))
ntx_p <- 0.000274 / (sum(bup_1, mmt_1, ntx_1, corres_1))
corres_p <- 0.996 / (sum(bup_1, mmt_1, ntx_1, corres_1))
bup_moud <- bup_1 / sum(bup_1, mmt_1, ntx_1)
mmt_moud <- mmt_1 / sum(bup_1, mmt_1, ntx_1)
ntx_moud <- ntx_1 / sum(bup_1, mmt_1, ntx_1)

block_trans[block_trans$initial_block == "section", "to_corresponding_post_trt_cycle56"] <- 0.215416759 * (1 - 0.077)
block_trans[block_trans$initial_block == "section", "to_section_cycle56"] <- (1 - 0.215416759)
block_trans[block_trans$initial_block == "section", "to_No_Treatment_cycle56"] <- 0
block_trans[block_trans$initial_block == "section", "to_Buprenorphine_cycle56"] <- 0.215416759 * 0.077 * bup_moud
block_trans[block_trans$initial_block == "section", "to_Methadone_cycle56"] <- 0.215416759 * 0.077 * mmt_moud
block_trans[block_trans$initial_block == "section", "to_Naltrexone_cycle56"] <- 0.215416759 * 0.077 * ntx_moud

# 65
block_trans[block_trans$initial_block == "section", "to_corresponding_post_trt_cycle65"] <- 0.215416759 * (1 - 0.077)
block_trans[block_trans$initial_block == "section", "to_section_cycle65"] <- (1 - 0.215416759)
block_trans[block_trans$initial_block == "section", "to_No_Treatment_cycle65"] <- 0
block_trans[block_trans$initial_block == "section", "to_Buprenorphine_cycle65"] <- 0.215416759 * 0.077 * bup_moud
block_trans[block_trans$initial_block == "section", "to_Methadone_cycle65"] <- 0.215416759 * 0.077 * mmt_moud
block_trans[block_trans$initial_block == "section", "to_Naltrexone_cycle65"] <- 0.215416759 * 0.077 * ntx_moud

# 104
# 90 days max for section, so kick everyone out here (starting wk 66)
block_trans[block_trans$initial_block == "section", "to_corresponding_post_trt_cycle104"] <- 1
block_trans[block_trans$initial_block == "section", "to_section_cycle104"] <- 0
block_trans[block_trans$initial_block == "section", "to_No_Treatment_cycle104"] <- 0
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

## post-blocks

## post blocks are easy
post_blocks <- list("post-diaspora", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "Post-Buprenorphine", "post-section")
post_blocks

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

block_trans[block_trans$initial_block == "post-diaspora", "to_corresponding_post_trt_cycle56"] <- 1
block_trans[block_trans$initial_block == "post-diaspora", "to_No_Treatment_cycle56"] <- 0

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

## select out cycle 156 columns
block_trans <- block_trans %>%
  select(-to_No_Treatment_cycle156, -to_Buprenorphine_cycle156, -to_Naltrexone_cycle156, -to_Methadone_cycle156, -to_corresponding_post_trt_cycle156)

write.csv(block_trans, paste0(input_folder,'/',"block_trans.csv", sep = ""), row.names = F)

}

### entering cohort ##########

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  enter_cohort <- read.csv(paste0(input_folder,'/',"entering_cohort.csv"))
  
enter_cohort[, c(4)] <- rep(0, length.out = 90)

enter_cohort <- enter_cohort %>%
  select(agegrp, sex, number_of_new_comers_cycle104)

enter_cohort <- enter_cohort %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(enter_cohort, paste0(input_folder,'/',"entering_cohort.csv", sep = ""), row.names = F)

}

### fatal od ##########

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  fatal_od <- read.csv(paste0(input_folder,'/',"fatal_overdose.csv"))
  
# change to rate, multiply, change back to proportion
updated_fatal_od <- 
  -ln(1 - fatal_od$fatal_to_all_types_overdose_ratio_cycle156)
updated_fatal_od <- updated_fatal_od * 2.3
updated_fatal_od <- 1 - (exp(-updated_fatal_od))

fatal_od[c(1), c(1)] <- updated_fatal_od
fatal_od[c(1), c(2)] <- updated_fatal_od

# multiply based on Josh's displacement paper
updated_fatal_od2 <- 
  -ln(1 - fatal_od$fatal_to_all_types_overdose_ratio_cycle156)
updated_fatal_od2 <- updated_fatal_od2 * 2.3 * 2.48
updated_fatal_od2 <- 1 - (exp(-updated_fatal_od2))

fatal_od <- fatal_od %>%
  mutate(fatal_to_all_types_overdose_ratio_cycle53 = updated_fatal_od2) %>%
  mutate(fatal_to_all_types_overdose_ratio_cycle56 = updated_fatal_od2) %>%
  mutate(fatal_to_all_types_overdose_ratio_cycle65 = updated_fatal_od)

fatal_od <- fatal_od %>%
  select(1, 4, 5, 6, 2, 3) %>%
  select(-6)

write.csv(fatal_od, paste0(input_folder,'/',"fatal_overdose.csv", sep = ""), row.names = F)

}

### oud trans ##########
# must sum to 1

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  oud_trans <- read.csv(paste0(input_folder,'/',"oud_trans.csv"))

# remove Detox and Post detox
oud_trans <- oud_trans %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox")

# add empty rows to oud_trans (base)
new_block13 <- oud_trans %>%
  filter(block == "No_Treatment")
new_block13[, c(5:8)] <- rep(0, length.out = 144)
new_block13$block <- "diaspora"

new_block14 <- oud_trans %>%
  filter(block == "No_Treatment")
new_block14[, c(5:8)] <- rep(0, length.out = 144)
new_block14$block <- "post-diaspora"

new_block15 <- oud_trans %>%
  filter(block == "No_Treatment")
new_block15[, c(5:8)] <- rep(0, length.out = 144)
new_block15$block <- "section"

new_block16 <- oud_trans %>%
  filter(block == "No_Treatment")
new_block16[, c(5:8)] <- rep(0, length.out = 144)
new_block16$block <- "post-section"

new_block17 <- oud_trans %>%
  filter(block == "No_Treatment")
new_block17[, c(5:8)] <- rep(0, length.out = 144)
new_block17$block <- "Corrections"

new_block18 <- oud_trans %>%
  filter(block == "No_Treatment")
new_block18[, c(5:8)] <- rep(0, length.out = 144)
new_block18$block <- "Post-Corrections"

oud_trans <- rbind(oud_trans, new_block13, new_block14, new_block15, new_block16, new_block17, new_block18)

# diaspora
oud_trans[oud_trans$block == "diaspora", 5:8] <- oud_trans[oud_trans$block == "No_Treatment", 5:8]

# section = detox
oud_trans[oud_trans$block == "section" & oud_trans$initial_status == "Active_Noninjection", "to_Active_Noninjection"] <- 1
oud_trans[oud_trans$block == "section" & oud_trans$initial_status == "Active_Injection", "to_Active_Injection"] <- 1
oud_trans[oud_trans$block == "section" & oud_trans$initial_status == "Nonactive_Noninjection", "to_Nonactive_Noninjection"] <- 1
oud_trans[oud_trans$block == "section" & oud_trans$initial_status == "Nonactive_Injection", "to_Nonactive_Injection"] <- 1

# corrections = no treatment
oud_trans[oud_trans$block == "Corrections", 5:8] <- oud_trans[oud_trans$block == "No_Treatment", 5:8]

# post diaspora = no treatment
oud_trans[oud_trans$block == "post-diaspora", 5:8] <- oud_trans[oud_trans$block == "No_Treatment", 5:8]

# post-section = post-bup
oud_trans[oud_trans$block == "post-section", 5:8] <- oud_trans[oud_trans$block == "Post-Buprenorphine", 5:8]

# post-Corrections = post-bup
oud_trans[oud_trans$block == "Post-Corrections", 5:8] <- oud_trans[oud_trans$block == "Post-Buprenorphine", 5:8]

options(max.print = 1300)
problems <- subset(oud_trans, rowSums((oud_trans[, c(5:8)])) != 1)
problems <- problems %>% mutate(sums = rowSums(problems[, 5:8]))

oud_trans <- oud_trans %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora",
      "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections",
      "post-section", "post-diaspora"
    )),
    agegrp, desc(sex), factor(initial_status, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

oud_trans <- oud_trans %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

# unique(oud_trans$block)

write.csv(oud_trans, paste0(input_folder,'/',"oud_trans.csv", sep = ""), row.names = F)

}

### SMR ##########

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  SMR <- read.csv(paste0(input_folder,'/',"SMR.csv"))

# add empty rows to SMR (base)
new_block19 <- SMR %>%
  filter(block == "No_Treatment")
new_block19[, c(5)] <- rep(0, length.out = 360)
new_block19$block <- "diaspora"

new_block20 <- SMR %>%
  filter(block == "No_Treatment")
new_block20[, c(5)] <- rep(0, length.out = 360)
new_block20$block <- "post-diaspora"

new_block21 <- SMR %>%
  filter(block == "No_Treatment")
new_block21[, c(5)] <- rep(0, length.out = 360)
new_block21$block <- "section"

new_block22 <- SMR %>%
  filter(block == "No_Treatment")
new_block22[, c(5)] <- rep(0, length.out = 360)
new_block22$block <- "post-section"

new_block23 <- SMR %>%
  filter(block == "No_Treatment")
new_block23[, c(5)] <- rep(0, length.out = 360)
new_block23$block <- "Corrections"

new_block24 <- SMR %>%
  filter(block == "No_Treatment")
new_block24[, c(5)] <- rep(0, length.out = 360)
new_block24$block <- "Post-Corrections"

SMR <- rbind(SMR, new_block19, new_block20, new_block21, new_block22, new_block23, new_block24)

# diaspora
SMR[SMR$block == "diaspora", "SMR"] <- SMR %>%
  filter(block == "Post-Buprenorphine") %>%
  select(SMR)
# section
SMR[SMR$block == "section", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)
# Corrections
SMR[SMR$block == "Corrections", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# post-diaspora
SMR[SMR$block == "post-diaspora", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)
# post-section
SMR[SMR$block == "post-section", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)
# post-corrections
SMR[SMR$block == "Post-Corrections", "SMR"] <- SMR %>%
  filter(block == "No_Treatment") %>%
  select(SMR)

# remove Detox and post-detox
SMR <- SMR %>% filter(block != "Detox")
SMR <- SMR %>% filter(block != "Post-Detox")
unique(SMR$block)

# percent change: bringing average value (4.66) to 9.8, all others up equivalently
# 9.8 is overall SMR
# remove fatal od (24.2%)
9.8 * (1 - 0.242)
(7.428 - 4.66) / 4.66
4.66 * 1.594

SMR$SMR <- SMR$SMR * (1.594)

SMR <- SMR %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17") %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora",
      "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections",
      "post-section", "post-diaspora"
    )),
    agegrp, desc(sex), factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

# unique(SMR$block)

write.csv(SMR, paste0(input_folder,'/',"SMR.csv", sep = ""), row.names = F)

}

###### UTILITIES #############################################################

## utility adjustments ##

nonhousing <- -0.0502
diaspora <- -0.02378

### bg_utility ###########

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  bg_utility <- read.csv(paste0(input_folder,'/',"cost_life/bg_utility.csv"))
  
bg_utility$utility <- bg_utility$utility + nonhousing
# adjust for not being housed

bg_utility <- bg_utility %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(bg_utility, paste0(input_folder,'/',"cost_life/bg_utility.csv", sep = ""), row.names = F)

}

### oud utility ##############

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  oud_utility <- read.csv(paste0(input_folder,'/',"/cost_life/oud_utility.csv"))
  
new_block26 <- oud_utility %>%
  filter(block == "No_Treatment")
new_block26$block <- "section"

new_block27 <- oud_utility %>%
  filter(block == "No_Treatment")
new_block27$block <- "diaspora"

new_block28 <- oud_utility %>%
  filter(block == "No_Treatment")
new_block28$block <- "post-section"

new_block29 <- oud_utility %>%
  filter(block == "No_Treatment")
new_block29$block <- "post-diaspora"

new_block30 <- oud_utility %>%
  filter(block == "No_Treatment")
new_block30$block <- "Corrections"

new_block31 <- oud_utility %>%
  filter(block == "Post-Buprenorphine")
new_block31$block <- "Post-Corrections"

oud_utility <- oud_utility %>%
  rbind(new_block26, new_block27, new_block28, new_block29, new_block30, new_block31)

oud_utility[oud_utility$block == "Corrections", 3] <- 1
oud_utility[oud_utility$block == "diaspora", 3] <- oud_utility[oud_utility$block == "No_Treatment", "utility"]
oud_utility[oud_utility$block == "section", 3] <- 1
oud_utility[oud_utility$block == "post-diaspora", 3] <- oud_utility[oud_utility$block == "No_Treatment", "utility"]
oud_utility[oud_utility$block == "post-section", 3] <- oud_utility[oud_utility$block == "Post-Corrections", "utility"]

oud_utility <- oud_utility %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox") %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora",
      "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections",
      "post-section", "post-diaspora")),
    factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

write.csv(oud_utility, paste0(input_folder,'/',"cost_life/oud_utility.csv", sep = ""), row.names = F)

}

### setting utility ###########

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  setting_utility <- read.csv(paste0(input_folder,'/',"cost_life/setting_utility.csv"))
  
# rename detox/post -> section/post
setting_utility$block[setting_utility$block == "Detox"] <- "section"
setting_utility$block[setting_utility$block == "Post-Detox"] <- "post-section"

# add diaspora/post
setting_utility <- setting_utility %>%
  add_row(block = 'diaspora',
          utility = 1) %>%
  add_row(block = "post-diaspora",
          utility = 1) %>%
  add_row(block = "Corrections",
          utility = 0.711) %>%
  add_row(block = "Post-Corrections",
          utility = 1)

# re-arrange
setting_utility <- setting_utility %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora",
      "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "post-section", "post-diaspora"))
  )

write.csv(setting_utility, paste0(input_folder,'/',"cost_life/setting_utility.csv", sep = ""), row.names = F)

}

##### COSTS ################################################################

### overdose cost ############

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  overdose_cost <- read.csv(paste0(input_folder,'/',"cost_life/overdose_cost.csv"))

# it's already correct, don't change anything

  write.csv(overdose_cost, paste0(input_folder,'/',"cost_life/overdose_cost.csv", sep = ""), row.names = F)
  
}

### pharmaceutical cost #############

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  pharmaceutical_cost <- read.csv(paste0(input_folder,'/',"cost_life/pharmaceutical_cost.csv"))
  
pharmaceutical_cost <- pharmaceutical_cost %>%
  filter(block != "Detox") %>%
  add_row(block = "Corrections",
          pharmaceutical_cost_.healthcare_system = 0) %>%
  add_row(block = "section",
          pharmaceutical_cost_.healthcare_system = 0) %>%
  add_row(block = "diaspora",
          pharmaceutical_cost_.healthcare_system = 0)

write.csv(pharmaceutical_cost, paste0(input_folder,'/',"cost_life/pharmaceutical_cost.csv", sep = ""), row.names = F)

}

### treatment utilization cost ##############

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  treatment_utilization_cost <- read.csv(paste0(input_folder,'/',"cost_life/treatment_utilization_cost.csv"))
  
treatment_utilization_cost <- treatment_utilization_cost %>%
  add_row(block = "Corrections",
          treatment_utilization_cost_.healthcare_system = 0) %>%
  add_row(block = "section",
          treatment_utilization_cost_.healthcare_system = 1477.23) %>%
  add_row(block = "diaspora",
          treatment_utilization_cost_.healthcare_system = 0) %>%
  filter(block != "Detox")

write.csv(treatment_utilization_cost, paste0(input_folder,'/',"cost_life/treatment_utilization_cost.csv", sep = ""), row.names = F)

}

### healthcare utilization cost ############

folders <- Sys.glob("input*")
print(folders)

for (input_folder in folders) {
  healthcare_utilization_cost <- read.csv(paste0(input_folder,'/',"cost_life/healthcare_utilization_cost.csv"))
  
  # this version of base_healthcare_utilization_cost doesn't have corrections

# add new blocks
new_block32 <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
new_block32$block <- "section"

new_block33 <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
new_block33$block <- "post-section"

new_block34 <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
new_block34$block <- "diaspora"

new_block35 <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
new_block35$block <- "post-diaspora"

new_block36 <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
new_block36$block <- "Corrections"

new_block37 <- healthcare_utilization_cost %>%
  filter(block == "No_Treatment")
new_block37$block <- "Post-Corrections"

healthcare_utilization_cost <- healthcare_utilization_cost %>%
  rbind(new_block32, new_block33, new_block34, new_block35, new_block36, new_block37)

# diaspora = average of post corrections and no treatment
healthcare_utilization_cost[healthcare_utilization_cost$block == "diaspora", 5] <-
  (healthcare_utilization_cost[healthcare_utilization_cost$block == "No_Treatment", 5] + healthcare_utilization_cost[healthcare_utilization_cost$block == "Post-Corrections", 5]) / 2

# corrections = 0
healthcare_utilization_cost[healthcare_utilization_cost$block == "Corrections", 5] <- 840

# section = 0
healthcare_utilization_cost[healthcare_utilization_cost$block == "section", 5] <- 0

healthcare_utilization_cost <- healthcare_utilization_cost %>%
  filter(block != "Detox") %>%
  filter(block != "Post-Detox") %>%
  arrange(
    factor(block, levels = c(
      "No_Treatment", "Buprenorphine", "Naltrexone", "Methadone", "Corrections", "section", "diaspora",
      "Post-Buprenorphine", "Post-Naltrexone", "Post-Methadone", "Post-Corrections", "post-section", "post-diaspora")),
    agegrp, desc(sex), 
    factor(oud, levels = c("Active_Noninjection", "Active_Injection", "Nonactive_Noninjection", "Nonactive_Injection"))
  )

healthcare_utilization_cost <- healthcare_utilization_cost %>%
  filter(agegrp != "10_11") %>%
  filter(agegrp != "12_13") %>%
  filter(agegrp != "14_15") %>%
  filter(agegrp != "16_17")

write.csv(healthcare_utilization_cost, paste0(input_folder,'/',"cost_life/healthcare_utilization_cost.csv", sep = ""), row.names = F)

}

##### READY TO RUN MODEL ####################################################
file.edit("codes/respond_main.R")
