### Update Block Transitions from No Trt to Corrections ###

# Last edited 12/28/2023
# Created by Hana Zwick
# Housing (MOUD Required) Strategy

# Corrections values are not calibrated, and must be updated manually. This script
# does so, running after the main strategy script. It calls a csv with the correct
# values and applies them to the relevant cells in the block_trans csv.

library(dplyr)

######################################################################

#### block_trans ####

 corrections_new <- read.csv("block_trans_corrections.csv")
 
 folders <- Sys.glob("input*")
 print(folders)
 
 for (input_folder in folders) {
   block_trans <- read.csv(paste0(input_folder,'/',"block_trans.csv"))

# replace values

# Male 18_19
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "18_19" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <-
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "18_19" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "18_19" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "18_19" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

print("made it through male 18/19")

# Female 18_19
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "18_19" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "18_19" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "18_19" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "18_19" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]
                    
print("made it through female 18/19")

# Male 20_21, 22_23
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "20_21" | block_trans$agegrp == "22_23") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "20_21" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "20_21" | block_trans$agegrp == "22_23") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "20_21" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]
                    
print("made it through male 20-23")

# Female 20_21, 22_23
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "20_21" | block_trans$agegrp == "22_23") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "20_21" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "20_21" | block_trans$agegrp == "22_23") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "20_21" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]
print("made it through female 20-23")

# Male 24_25
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "24_25" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "24_25" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "24_25" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "24_25" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]
                    
print("made it through male 24-25")

# Female 24_25
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "24_25" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "24_25" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "24_25" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "24_25" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

print("made it through female 24-25")

# Male 26_27, 28_29, 30_31, 32_33
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "26_27" | block_trans$agegrp == "28_29" |
               block_trans$agegrp == "30_31" | block_trans$agegrp == "32_33") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "26_27" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "26_27" | block_trans$agegrp == "28_29" |
               block_trans$agegrp == "30_31" | block_trans$agegrp == "32_33") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "26_27" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Female 26_27, 28_29, 30_31, 32_33
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "26_27" | block_trans$agegrp == "28_29" |
               block_trans$agegrp == "30_31" | block_trans$agegrp == "32_33") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "26_27" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "26_27" | block_trans$agegrp == "28_29" |
               block_trans$agegrp == "30_31" | block_trans$agegrp == "32_33") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "26_27" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

print("made it through female 26-33")

# Male 34_35
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "34_35" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "34_35" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "34_35" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "34_35" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Female 34_35
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "34_35" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "34_35" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "34_35" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "34_35" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

print("made it through female 34-35")

# Male 36_37, 38_39, 40_41, 42_43
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "36_37" | block_trans$agegrp == "38_39" |
               block_trans$agegrp == "40_41" | block_trans$agegrp == "42_43") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "36_37" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "36_37" | block_trans$agegrp == "38_39" |
               block_trans$agegrp == "40_41" | block_trans$agegrp == "42_43") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "36_37" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Female 36_37, 38_39, 40_41, 42_43
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "36_37" | block_trans$agegrp == "38_39" |
               block_trans$agegrp == "40_41" | block_trans$agegrp == "42_43") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "36_37" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "36_37" | block_trans$agegrp == "38_39" |
               block_trans$agegrp == "40_41" | block_trans$agegrp == "42_43") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "36_37" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Male 44_45
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "44_45" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "44_45" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "44_45" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "44_45" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Female 44_45
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "44_45" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "44_45" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "44_45" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "44_45" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Male 46_47, 48_49, 50_51, 52_53
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "46_47" | block_trans$agegrp == "48_49" |
               block_trans$agegrp == "50_51" | block_trans$agegrp == "52_53") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "46_47" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "46_47" | block_trans$agegrp == "48_49" |
               block_trans$agegrp == "50_51" | block_trans$agegrp == "52_53") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "46_47" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Female 46_47, 48_49, 50_51, 52_53
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "46_47" | block_trans$agegrp == "48_49" |
               block_trans$agegrp == "50_51" | block_trans$agegrp == "52_53") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "46_47" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "46_47" | block_trans$agegrp == "48_49" |
               block_trans$agegrp == "50_51" | block_trans$agegrp == "52_53") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "46_47" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Male 54_55
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "54_55" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "54_55" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              block_trans$agegrp == "54_55" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "54_55" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Female 54_55
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "54_55" &
              (block_trans$oud == "Active_Noninjection" | block_trans$oud == "Active_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "54_55" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              block_trans$agegrp == "54_55" &
              (block_trans$oud == "Nonactive_Noninjection" | block_trans$oud == "Nonactive_Injection"), 
              "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "54_55" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Male 56_57, 58_59, 60_61, 62_63, 64_65, 66_67, 68_69, 70_71, 72_73, 74_75, 
# 76_77, 78_79, 80_81, 82_83, 84_85, 86_87, 88_89, 90_91, 92_93, 94_95, 96_97, 98_99
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "56_57" | block_trans$agegrp == "58_59" |
               block_trans$agegrp == "60_61" | block_trans$agegrp == "62_63" |
               block_trans$agegrp == "64_65" | block_trans$agegrp == "66_67" |
               block_trans$agegrp == "68_69" | block_trans$agegrp == "70_71" |
               block_trans$agegrp == "72_73" | block_trans$agegrp == "74_75" |
               block_trans$agegrp == "76_77" | block_trans$agegrp == "78_79" |
               block_trans$agegrp == "80_81" | block_trans$agegrp == "82_83" |
               block_trans$agegrp == "84_85" | block_trans$agegrp == "86_87" |
               block_trans$agegrp == "88_89" | block_trans$agegrp == "90_91" |
               block_trans$agegrp == "92_93" | block_trans$agegrp == "94_95" |
               block_trans$agegrp == "96_97" | block_trans$agegrp == "98_99") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "56_57" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Male" &
              (block_trans$agegrp == "56_57" | block_trans$agegrp == "58_59" |
               block_trans$agegrp == "60_61" | block_trans$agegrp == "62_63" |
               block_trans$agegrp == "64_65" | block_trans$agegrp == "66_67" |
               block_trans$agegrp == "68_69" | block_trans$agegrp == "70_71" |
               block_trans$agegrp == "72_73" | block_trans$agegrp == "74_75" |
               block_trans$agegrp == "76_77" | block_trans$agegrp == "78_79" |
               block_trans$agegrp == "80_81" | block_trans$agegrp == "82_83" |
               block_trans$agegrp == "84_85" | block_trans$agegrp == "86_87" |
               block_trans$agegrp == "88_89" | block_trans$agegrp == "90_91" |
               block_trans$agegrp == "92_93" | block_trans$agegrp == "94_95" |
               block_trans$agegrp == "96_97" | block_trans$agegrp == "98_99") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Male" &
                    corrections_new$agegrp == "56_57" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]

# Female 56_57, 58_59, 60_61, 62_63, 64_65, 66_67, 68_69, 70_71, 72_73, 74_75, 
# 76_77, 78_79, 80_81, 82_83, 84_85, 86_87, 88_89, 90_91, 92_93, 94_95, 96_97, 98_99
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "56_57" | block_trans$agegrp == "58_59" |
               block_trans$agegrp == "60_61" | block_trans$agegrp == "62_63" |
               block_trans$agegrp == "64_65" | block_trans$agegrp == "66_67" |
               block_trans$agegrp == "68_69" | block_trans$agegrp == "70_71" |
               block_trans$agegrp == "72_73" | block_trans$agegrp == "74_75" |
               block_trans$agegrp == "76_77" | block_trans$agegrp == "78_79" |
               block_trans$agegrp == "80_81" | block_trans$agegrp == "82_83" |
               block_trans$agegrp == "84_85" | block_trans$agegrp == "86_87" |
               block_trans$agegrp == "88_89" | block_trans$agegrp == "90_91" |
               block_trans$agegrp == "92_93" | block_trans$agegrp == "94_95" |
               block_trans$agegrp == "96_97" | block_trans$agegrp == "98_99") &
              (block_trans$oud == "Active_Injection" | block_trans$oud == "Active_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "56_57" &
                    corrections_new$oud == "Active_Noninjection", "to_Corrections_cycle936"]
block_trans[block_trans$initial_block == "No_Treatment" & 
              block_trans$sex == "Female" &
              (block_trans$agegrp == "56_57" | block_trans$agegrp == "58_59" |
               block_trans$agegrp == "60_61" | block_trans$agegrp == "62_63" |
               block_trans$agegrp == "64_65" | block_trans$agegrp == "66_67" |
               block_trans$agegrp == "68_69" | block_trans$agegrp == "70_71" |
               block_trans$agegrp == "72_73" | block_trans$agegrp == "74_75" |
               block_trans$agegrp == "76_77" | block_trans$agegrp == "78_79" |
               block_trans$agegrp == "80_81" | block_trans$agegrp == "82_83" |
               block_trans$agegrp == "84_85" | block_trans$agegrp == "86_87" |
               block_trans$agegrp == "88_89" | block_trans$agegrp == "90_91" |
               block_trans$agegrp == "92_93" | block_trans$agegrp == "94_95" |
               block_trans$agegrp == "96_97" | block_trans$agegrp == "98_99") &
              (block_trans$oud == "Nonactive_Injection" | block_trans$oud == "Nonactive_Noninjection"), 
            "to_Corrections_cycle52"] <- 
                    corrections_new[corrections_new$initial_block == "No_Treatment" &
                    corrections_new$sex == "Female" &
                    corrections_new$agegrp == "56_57" &
                    corrections_new$oud == "Nonactive_Noninjection", "to_Corrections_cycle936"]



# copy to other time points

block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle53"] <- 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle56"] <- 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle65"] <- 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle104"] <- 
    block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"]
    
print("copied to all time points")



# make sure everything sums to 0
    # change no trt -> no trt
    
# 52
block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle52"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Buprenorphine_cycle52"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Naltrexone_cycle52"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Methadone_cycle52"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle52"])

# 53
block_trans[block_trans$initial_block == "No_Treatment", "to_diaspora_cycle53"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle53"])
   
# 56
block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle56"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle56"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle56"])

# 65
block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle65"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle65"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle65"])

# 104
block_trans[block_trans$initial_block == "No_Treatment", "to_No_Treatment_cycle104"] <-
  (1 - block_trans[block_trans$initial_block == "No_Treatment", "to_housing_meds_cycle104"]
   - block_trans[block_trans$initial_block == "No_Treatment", "to_Corrections_cycle104"])


# write out block_trans.csv
write.csv(block_trans, paste0(input_folder,'/',"block_trans.csv", sep = ""), row.names = F)

 }

## ***update main scripts to make but not touch corrections***


print("vita!")


