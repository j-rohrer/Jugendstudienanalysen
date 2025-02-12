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

prop.table(table(youth2023$schooltype[youth2023$schule_amt51 %in% have_tablet & 
                                        youth2023$rawid %in% combined$id]))
prop.table(table(youth2023$schooltype[youth2023$schule_amt51 %in% have_paper & 
                                        youth2023$rawid %in% combined$id]))
# 1 Mittelschule
# 2 Gymnasium
# 3 Berufs-/Fachoberschule
# 4 Förderschule

# With tablet: More Mittelschule
# With Paper: More Gymnasium, Berufs/Fachoberschule, Förderschule



# does the interaction disappear if we split by schooltype

mittel <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$schooltype == 1,], weights = wph)
summary(mittel)

gym <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$schooltype == 2,], weights = wph)
summary(gym)

# No: we have the pattern in both
# so this aint it chief


# migration background
# this clearly does not vary systematically

prop.table(table(youth2023$migback[youth2023$rawid %in% combined$id], youth2023$source[youth2023$rawid %in% combined$id]), margin = 2)


# looks like the same age distribution
summary(youth2023$age[youth2023$source == "paper" & youth2023$rawid %in% combined$id])
summary(youth2023$age[youth2023$source == "online" & youth2023$rawid %in% combined$id])


prop.table(table(youth2023$gender[youth2023$rawid %in% combined$id], youth2023$source[youth2023$rawid %in% combined$id]), margin = 2)
# looks about the same



# number of siblings
prop.table(table(youth2023$f32_diff[youth2023$rawid %in% combined$id], youth2023$source[youth2023$rawid %in% combined$id]), margin = 2)
# slightly larger families on paper
table(youth2023$f32_diff)

#################
# to check
# does the interaction disappear by family size?
#################


# I will have to clean up these
prop.table(table(youth2023$plz[youth2023$rawid %in% combined$id], youth2023$source[youth2023$rawid %in% combined$id]), margin = 2)
unique(youth2023$plz)
table(youth2023$plz_diff)
prop.table(table(youth2023$plz_diff[youth2023$rawid %in% combined$id], youth2023$source[youth2023$rawid %in% combined$id]), margin = 2)

# more paper in plz diff 15, 16
# I will have to plot a map or something

# parental work
prop.table(table(youth2023$f33a[youth2023$rawid %in% combined$id], youth2023$source[youth2023$rawid %in% combined$id]), margin = 2) # vater
prop.table(table(youth2023$f33b[youth2023$rawid %in% combined$id], youth2023$source[youth2023$rawid %in% combined$id]), margin = 2) # mutter

# looks approximately the same!

table(youth2023$f33a) # Vater
table(youth2023$f33b) # Mutter

#################
# next step
#################

# do a map to figure out where people are from
