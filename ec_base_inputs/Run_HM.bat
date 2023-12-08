@echo off

for /l %%x in (1, 1, 5) do (
	robocopy "input%%x" "../hm_inputs/input%%x" /s /e
)

cd ../hm_inputs

Rscript HM_GitHub.R

Rscript cp_files.R



:: run RESPOND
for /l %%x in (1, 1, 5) do (
	mkdir "output%%x"
	Rscript src/respond_main.R %%x 1
	move /y "output%%x" "../hm_outputs/output%%x"
)







