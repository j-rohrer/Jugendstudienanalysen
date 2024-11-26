####################
# Could the differences
# in sampling explain the
# gender gap?
####################

# DO NOT FORGET MODE DIFFERENCES
table(youth2023$source) # this is almost half online and half paper!

load("prep.RData")

# Contact Gregor for confirmation
# then: calculate ICCs
# Look at school-level gender diffs
# Simulate: We start drawing from schools with larger
# gender diffs, can that explain the pattern


#####################
# Generate unique classroom ID
#####################

# youth2010: schule, klasse A/B/C
table(youth2010$klasse)
# Suspicion: Klasse within Schule identifies unique classrooms
youth2010$unique_classroom <- paste0(youth2010$schule, youth2010$klasse)
table(youth2010$unique_classroom)
length(unique(youth2010$unique_classroom))
# 114 classrooms in
table(youth2010$schule)
length(unique(youth2010$schule)) # 38 schools
str(youth2010$schule)
youth2010$school <- paste("2011", youth2010$schule, sep = "_")
youth2010$unique_classroom <- paste("2011", youth2010$unique_classroom, sep = "_")

# youth2015
# kennung, schulart, ortsteil, Klasse
# class from 1 to 11
length(unique(youth2015$kennung))
# 114 distinct values -- classrooms
# this variable is schulart + ortsteil + klasse and should identify unique classrooms
# schulart + ortsteil should uniquely identify schools
# only exception: documentation looks like there are two Berufs/fachoberschulen
# in 02 (Zentrum-Suedost)
youth2015$school <- paste0("2015_", youth2015$ortsteil, youth2015$schulart)
length(unique(youth2015$school))
table(youth2015$school)
youth2015$school[youth2015$school == "NANA"] <- NA
youth2015$unique_classroom <- paste(2015, youth2015$kennung, sep = "_")


# youth2023
names(youth2023)
# klassenstufe, schulart, schule_amt51, klasse_amt51, schulart
table(youth2023$schule_amt51, useNA = "always")
table(youth2023$klasse_amt51, useNA = "always")
youth2023$test <- paste(youth2023$schule_amt51, youth2023$klasse_amt51)
length(unique(youth2023$test)) # 168
# SANITY CHECK: CONTACT GREGOR



youth2023$klassenstufe
youth2023$klassenstufe2

library(lme4)
icc <- lmer(satis ~ 1 + (1|schule/unique_classroom), data = youth2010)

summary(icc)
vc <- as.data.frame(VarCorr(icc))

school_var <- vc[vc$grp == "schule", "vcov"]
classroom_var <- vc[vc$grp == "unique_classroom:schule", "vcov"]
residual_var <- vc[vc$grp == "Residual", "vcov"]

# Calculate ICCs
icc_school <- school_var / (school_var + classroom_var + residual_var) # 0.011
icc_classroom_rel_to_total <- classroom_var / (classroom_var + school_var + residual_var) # 0.03
icc_classroom_inc_school <- (classroom_var + school_var) / (classroom_var + school_var +residual_var) # 0.041

icc_residual <- residual_var / (school_var + classroom_var + residual_var)