####################
# Find out what is different about
# the schools with/without tablets
####################


load("prep.RData")


####################
# How are the schools with tablets different?
####################
# Have tablet
have_tablet <- unique(youth2023$schule_amt51[youth2023$source == "online"])
have_paper <- unique(youth2023$schule_amt51[youth2023$source == "paper"])

prop.table(table(youth2023$schooltype[youth2023$schule_amt51 %in% have_tablet]))
prop.table(table(youth2023$schooltype[youth2023$schule_amt51 %in% have_paper]))
# 1 Mittelschule
# 2 Gymnasium
# 3 Berufs-/Fachoberschule
# 4 Förderschule

# With tablet: More Mittelschule
# With Paper: More Gymnasium, Berufs/Fachoberschule, Förderschule

# no, lets work from the combined data

prop.table(table(combined$schooltype[combined$source == "online"]))
prop.table(table(combined$schooltype[combined$source == "paper"]))

# does the interaction disappear if we split by schooltype

mittel <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$schooltype == 1,], weights = wph)
summary(mittel)

gym <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$schooltype == 2,], weights = wph)
summary(gym)

# No: we have the pattern in both
# so this aint it chief