# This script copies the relevant user_inputs.R and input_file_paths.R files
# into every input in the current folder (this is needed to run RESPOND).

# Run this from the strategy-specific input folder.


folders <- Sys.glob("input*")
print(folders)
currentfiles <- c("user_inputs.R", "input_file_paths.R")


for (input_folder in folders) {
  for (file_to_copy in currentfiles) {
    dest_file <- file.path(input_folder, file_to_copy)
    file.copy(from = file_to_copy, to = dest_file, 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
  }
}