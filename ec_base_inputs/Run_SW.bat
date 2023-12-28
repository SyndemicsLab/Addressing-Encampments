@echo off

for /l %%x in (1, 1, 5) do (
	robocopy "input%%x" "../sw_inputs/input%%x" /s /e
)

cd ../sw_inputs

Rscript SW_GitHub.R

Rscript corrections_update.R

Rscript cp_files.R



:: run RESPOND
for /l %%x in (1, 1, 5) do (
	mkdir "output%%x"
	Rscript src/respond_main.R %%x 1
	move /y "output%%x" "../sw_outputs/output%%x"
)







