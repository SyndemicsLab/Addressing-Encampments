@echo off

for /l %%x in (1, 1, 1000) do (
	robocopy "input%%x" "../sq_inputs/input%%x" /s /e
)

cd ../sq_inputs

Rscript SQ_GitHub.R

Rscript cp_files.R



:: run RESPOND
for /l %%x in (1, 1, 1000) do (
	mkdir "output%%x"
	Rscript src/respond_main.R %%x 1
	move /y "output%%x" "../sq_outputs/output%%x"
)







