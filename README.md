# Welcome to the RESPOND Sweeps Project

## Introduction
Titled "Mortality and Economic Impact of a Police Sweep on People Who are Unsheltered and Who Use Opioids" and colloquially called "Sweeps", this project uses the RESPOND model to model the mortality and health-economic impacts of a street sweep on a cohort of individuals who are unhoused and use opioids.

## RESPOND Model
Brief description, point to RESPOND v1 repository

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
**4. Run the Status Quo script, put the resulting 1,000 input sets into the status_quo inputs folder**
**5. Copy user_inputs.R, input_file_paths.R, and the results script into each input set**
**6. Run RESPONDv1, copying outputs into the status_quo outputs folder**
**7. Run the results script to calculate results**
**8. Repeat steps 4-7 for the other strategies: Sweep, Housing with MOUD Required, and Housing with MOUD Choice**
