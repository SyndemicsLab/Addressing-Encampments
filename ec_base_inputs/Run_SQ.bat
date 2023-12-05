@echo off

for /l %%x in (1, 1, 5) do (
	robocopy "input%%x" "../sq_inputs/input%%x" /s /e
)

cd ../sq_inputs

Rscript cp_files.R

Rscript SQ_GitHub.R



:: run RESPOND
for /l %%x in (1, 1, 5) do (
	mkdir "output%%x"
	Rscript src/respond_main.R %%x 1
	robocopy "output%%x" "../respond_sweeps_public/sq_outputs/output%%x" /s /e
)







