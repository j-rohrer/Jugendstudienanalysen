##########################
# Can we find a proper school
# variable for 2015
##########################



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

# Check consistency across years
load("prep.RData")
library(lme4)
library(lmerTest)

school <- lmer(satis ~ (1|school_id) + (1|year), data = combined[!is.na(combined$school_id) & !is.na(combined$year),]
)
noschool <- lmer(satis ~ (1|year), data = combined[!is.na(combined$school_id) & !is.na(combined$year),]
)

anova(school, noschool)

summary(school)
0.007929/(0.007929 + 0.015556+ 0.700716)
# like one percent of the variability