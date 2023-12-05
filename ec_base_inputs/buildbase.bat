@echo off


cd ../../RESPONDv1

for /l %%x in (1, 1, 5) do (
	Rscript ./calibration/R/ec_base.R %%x %%x
	robocopy "input%%x" "../respond_sweeps_public/ec_base_inputs/input%%x" /s /e
)

cd ../respond_sweeps_public/ec_base_inputs
