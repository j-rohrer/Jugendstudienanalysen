##########################
# Can we find a proper school
# variable for 2015
##########################

load("prep.RData")


library(haven)

# jugend2015_asc
test <- read_spss("Files/jugend2015_asc.sav")
# jugend2015_kl
test <- read_spss("Files/jugend2015_kl.sav")

# jugend2015_R
test <- read_spss("Files/jugend2015_R.sav")

# jugend2015
test <- read_spss("Files/jugend2015.sav")

# jugend2015schule
test <- read_spss("Files/jugend2015schule.sav")
table(test$schule, test$schulart)

test$schule_manuell <- paste(test$ortsteil, test$schulart, sep = "_")

check <- test[, c("Klasse", "kennung", "schule", "schulart", "schule_manuell")]

check$schule_neu <- substr(test$kennung, 1, 3)
table(check$schule_neu, check$schulart)

# I think that's it, the correct school variable