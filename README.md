# Welcome to the RESPOND Sweeps Project

## Introduction
Titled "Mortality and Economic Impact of a Police Sweep on People Who are Unsheltered and Who Use Opioids" and colloquially called "Sweeps", this project uses the RESPOND model to model the mortality and health-economic impacts of a street sweep on a cohort of individuals who are unhoused and use opioids.

## RESPOND Model
Brief description, point to RESPOND v1 repository

## Dependencies
**R**
-> Make sure Rscript is on PATH  
**Git**

## Instructions
**1. Clone this repository**
- On GitHub.com, navigate to the main page of the repository. Above the list of files, click **<> Code**.
- Copy the URL for the repository.
- Open Git Bash, and navigate the working directory to the location where you want the cloned directory.
- Type *git clone*, and then paste the URL you copied earlier.  
```
git clone https://github.com/SyndemicsLab/respond_sweeps_public.git
```
- Press **Enter** to create your local clone.

**2. Clone the RESPONDv1 repository in the same place**
Follow the same instructions as #1, with the RESPONDv1 repository.
```
git clone https://github.com/SyndemicsLab/RESPONDv1.git
```

**3. Use the RESPONDv1 repository to generate inputs 1 - 1,000**
- Navigate to RESPONDv1.
- For Windows, open Command Prompt as an administrator. For Unix,
- Navigate to the base inputs folder.
```
cd C:\YOURFILEPATH\respond_sweeps_public\ec_base_inputs
```
- Run *buildbase.bat* using the following command. This will create 1,000 input sets and copy them into respond_sweeps_public/ec_base_inputs
```
buildbase.bat
```
- This will take a while! Maybe go for a walk or make yourself a little snack.

**4. Update the age structure**
- For Windows, go back to your Command Prompt. Navigate to the 'ec_base_inputs' folder, and run the age_chunks_5_to_2 script to update the age structure of your base 1,000 files.
- For Unix, 
```
Rscript age_chunks_5_to_2.R
```
- This will also take a while. Do you have a pet in need of some attention?

**5. Run the Status Quo script, put the resulting 1,000 input sets into the status quo inputs folder**
- Navigate to the 'sq_inputs' folder.
- Run the script 'something_sq.R'. This will read the base inputs, update them to reflect the status quo strategy, and populate them into the 'sq_inputs' folder.
```
Rscript something_sq.R
```

**6. Copy user_inputs.R and input_file_paths.R into each input set**
- Still in the 'sq_inputs' folder, run 'cp_files.r' to copy the two necessary Rscripts into each input folder.
- The necessary R scripts are 'user_inputs.R' and 'input_file_paths.R', both of which are pre-set and in the strategy-specific folders.
```
Rscript cp_files.R
```

**7. Run RESPONDv1, copying outputs into the status quo outputs folder**
- This is how to run RESPONDv1
- Make sure the outputs go to the 'sq_outputs' folder

**8. Run the results script to calculate results**
- Script is called:
- Script does the following:
- This is how to run the script: 

**9. Repeat steps 5-8 for the other strategies: Sweep, Housing with MOUD Required, and Housing with MOUD Choice**
- Make sure to put the inputs and outputs in the right folders
