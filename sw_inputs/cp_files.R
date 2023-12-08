# This script copies the relevant user_inputs.R and input_file_paths.R files
# into every input in the current folder (this is needed to run RESPOND).

# Run this from the strategy-specific input folder.


folders <- Sys.glob("input*")
print(folders)
currentfiles <- c("user_inputs.R", "input_file_paths.R")


for (input_folder in folders) {
  for (file_to_copy in currentfiles) {
    file_in <- paste0("./../setup_files/SW/", file_to_copy)
    file_out <- paste0("./", input_folder, "/")
    file.copy(from = file.path(file_in), to = file_out, 
              overwrite = TRUE, copy.date = TRUE)
  }
} 
