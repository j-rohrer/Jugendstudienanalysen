####################
# Look at demographic differences
# for real this time
####################

# migration background, all outcomes
# age
# school type
# also do school fixed effects
# make some nice plots

load("prep.RData")

# migback
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$german_at_home == 0,], weights = wph)
summary(simple_model) # sign

simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$german_at_home == 1,], weights = wph)
summary(simple_model) # sign



simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$migback == "both",], weights = wph)
summary(simple_model) # sign
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$migback == "father",], weights = wph)
summary(simple_model) # ns but same sign, .19
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$migback == "mother",], weights = wph)
summary(simple_model) # ns but same sign, .66
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$migback == "none",], weights = wph)
summary(simple_model) # .22 and sign
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$migback == "self",], weights = wph)
summary(simple_model) # .744 and sign

# schooltype
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$schooltype == 1,], weights = wph)
summary(simple_model) # sign

simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$schooltype == 2,], weights = wph)
summary(simple_model) # sign

# add age

simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + age*as.factor(gender), data = combined[combined$schooltype == 1,], weights = wph)
summary(simple_model) # sign






load("prep.RData")


simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined, weights = wph)
summary(simple_model)

mean(combined$german_at_home[combined$year == 2010], na.rm = TRUE) # .90
mean(combined$german_at_home[combined$year == 2015], na.rm = TRUE) # .905
mean(combined$german_at_home[combined$year == 2023], na.rm = TRUE) # .87

# is this shift plausible given the population?
ewo <- readRDS("Files/Ewo_PLZ_Geschl_Migration.rds")

ewo <- ewo[ewo$Alter >= 12 & ewo$Alter <= 18, ]
table(ewo$Zuwanderungshintergrund)
