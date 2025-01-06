################
# Minimal example
# Reproduce the schulart schule mismatch 2015
################

library(haven)
youth2015_school <- read_spss("Files/jugend2015Schule.sav")
table(youth2015_school$schule, youth2015_school$schulart)

# z.B. 104: Thomasschule Gymnasium als drei Schularten aufgeführt
# 265: 94. Oberschule als alle vier Schularten aufgeführt