@echo off

for /l %%x in (1, 1, 5) do (
	robocopy "input%%x" "../hc_inputs/input%%x" /s /e
)

cd ../hc_inputs

Rscript HC_GitHub.R

Rscript cp_files.R



:: run RESPOND
for /l %%x in (1, 1, 5) do (
	mkdir "output%%x"
	Rscript src/respond_main.R %%x 1
	move /y "output%%x" "../hc_outputs/output%%x"
)







