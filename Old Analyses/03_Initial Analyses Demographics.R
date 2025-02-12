######################
# Let's figure out whether it could be demographics
######################
# NEXT STEP
# Fold additional variables into data cleaning
# Geschwister
# Wohnsituation
# Gewichte
# combined dataset
# Restrict to male, female
# restrict to ages between 12 and 18 (or 19?)
# add value labels also to schooltype

load("prep.RData")
youth2010$wph <- 1
youth2015$wph <- 1
vars <- c("year", "wph", "schooltype", "gender", "age", "german_at_home", 
          "satis", "satis_money", "satis_friends", "satis_mom", "satis_dad",
          "satis_leisure", "satis_dwell", "satis_grades")
combined <- rbind(youth2010[, vars], youth2015[, vars], youth2023[, vars])
combined$gender[combined$gender == "diverse"] <- NA
combined <- combined[!is.na(combined$gender),]
# Could it be age?



# Age differences between the years
summary(lm(age ~ as.factor(year), data = combined, weights = wph))

# Restrict to narrower age range
summary(lm(age ~ as.factor(year), data = combined[combined$age >= 12 & combined$age <= 18,], weights = wph))





library(marginaleffects)



library(marginaleffects)


combined <- combined[!is.na(combined$age),]
combined <- combined[combined$age >= 12 & combined$age <= 18,]
# model with age


age_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + 
                  age*as.factor(gender) + I(age^2)*as.factor(gender) + I(age^3)*as.factor(gender), 
                data = combined[combined$age >= 12 & combined$age <= 18,], 
                weights = wph)
summary(age_model)

# Additionally consider schooltype
prelim_model <- lm(scale(satis) ~ age*as.factor(gender) + I(age^2)*as.factor(gender) + I(age^3)*as.factor(gender) +
                    as.factor(schooltype)*as.factor(gender)*as.factor(year) , 
                data = combined, 
                weights = wph)
summary(prelim_model)

# still significant with age up to age3 and its interaction with gender
# still significant with schooltype and its interaction with gender
# still significant: age*gender up to ^3, schooltype*gender*age

prelim_model <- lm(scale(satis) ~ age*as.factor(gender) + I(age^2)*as.factor(gender) + I(age^3)*as.factor(gender) +
                     as.factor(schooltype)*as.factor(gender)*as.factor(year)*as.factor(german_at_home), 
                   data = combined, 
                   weights = wph)
summary(prelim_model)

# with migration: larger effect size, borderline significant
# at this point we do have to take into account scaling etc tho
library(marginaleffects)
pred_prelim_model <- predictions(prelim_model,
                              by = c("gender", "year"))



# first, let's check how age relates to the outcome
library(ggplot2)
library(haven)
ggplot(data = combined, aes(x = age, y = satis, group = gender, color = gender)) +
  geom_smooth(method = "loess") +
  theme_classic()


table(combined$age, combined$gender)

age_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + 
                  age*as.factor(gender) + I(age^2)*as.factor(gender) + I(age^3)*as.factor(gender), 
                   data = combined[combined$age >= 12 & combined$age <= 18,], 
                   weights = wph)
summary(age_model)

pred_age_model <- predictions(age_model,
                    by = c("gender", "year"))

# What changes without age in the model?

no_age_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) , 
                data = combined[combined$age >= 12 & combined$age <= 18,], 
                weights = wph)
summary(no_age_model)

pred_no_age_model <- predictions(no_age_model,
                              by = c("gender", "year"))

pred_age_model
pred_no_age_model

# These estimates are virtually identical
# so I think they are marginalizing over the actual age distribution

# New data grid

pred_age_model <- predictions(age_model,
                              by = c("gender", "year"),
                              newdata = "mean")

pred_no_age_model <- predictions(no_age_model,
                                 by = c("gender", "year"),
                                 newdata = "mean")

# Effect for people at average age
pred_age_model
pred_no_age_model

# Should we allow for more interactions?



# Look within schooltypes
# Anzahl Geschwister?
# Eltern geschieden ja nein?
# Project to general population? Project to population of 2010?
  
#############
# Geschwister
#############
# Anzahl geschwister
# 2010: nein, ja 1, ja 2, ja 3 oder mehr
library(haven)

table(youth2010$s28)
youth2010$sibs <- youth2010$s28
table(youth2010$sibs)
# 2015: nein, ja 1, ja 2, ja 3 oder mehr
table(youth2015$f21)
youth2015$sibs <- youth2015$f21 - 1
table(youth2015$sibs)
# 2023: enter number of siblings
table(youth2023$f32)
youth2023$sibs <- ifelse(youth2023$f32 < 4, youth2023$f32, 3)
table(youth2023$sibs)

#############
# Mit Eltern zusammen
#############
table(youth2010$s27)
# known coding (labelled in questionnaire)
# 1: in einer Familie mit meinen Eltern
# 2: in einer Familie mit einem Elternteil und dessen Partner
# 3: in einer Familie mit nur einem Elternteil
# 4: bei den Großeltern
# 5: in einer Wohnung/Wohngemeinschaft
# 6: nicht in einer Familie
# 7: woanders
summary(youth2010$age[youth2010$s27 == 1])
summary(youth2010$age[youth2010$s27 == 5]) # checks out, much older

table(youth2015$f20)
# best guess (matches previous year)
# 1: in einer Familie mit meinen Eltern
# 2: in einer Familie mit einem Elternteil und dessen Partner
# 3: in einer Familie mit nur einem Elternteil
# 4: bei den Großeltern
# 5: in einer Wohnung/Wohngemeinschaft
# 6: nicht in einer Familie
# 7: woanders, und zwar

table(youth2023$hhstat)
# from codeplan
# 1: bei meinen Eltern
# 2: abwechselnd bei einem Elternteil (Wechselmodell)
# 3: bei einem Elternteil mit Partner*in
# 4: mit nur einem Elternteil
# 5: in einer Wohngruppe/Internat
# 6: in einer eigenen Wohnung/WG
# 7: woanders, und zwar



#############
# Erwerbssituation Eltern
#############

table(youth2010$s29a) # Vater
table(youth2010$s29b) # Mutter
# nach codeplan angeblich
# 1: ganztätig berufstätig
# 2: nicht ganztätig berufstätig
# 3: zur Zeit arbeitslos
# 4: Rentner
# 5: Hausmann/Hausfrau
# 6: in Ausbildung, Studium, Weiterbildung
# 7: weiß ich nicht

table(youth2015$f22a) # Vater
table(youth2015$f22b) # Mutter
# vermutlich wie 2023 aber
# weiß nicht codiert mit -99

table(youth2023$f33a) # Vater
table(youth2023$f33b) # Mutter
# nach Codeplan
# 1: ganztags berufstätig
# 2: nicht ganztags/Teilzeit
# 3: arbeitslos
# 4: Hausmann/Hausfrau/in Elternzeit
# 5: in Ausbildung, Studium, Weiterbildung
# 6: Rentner*in
# 7: weiß nicht/trifft nicht zu

# 2010: @beruf_neu, @berufElt
# table(youth2010$s29a) # Vater
# table(youth2010$s29b) # Mutter
table(youth2010$`@beruf_neu`)
table(youth2010$`@berufElt`)
table(youth2010$s29a)
table(youth2010$s29b)

# Look within schooltypes


# Project to general population? Project to population of 2010?


##################################
# Reconstruct within the data
# School, classes, grade levels
##################################

#####################
# youth2010
#####################

# ICCs: How much is explained through the cluster variables?
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
youth2010$schule <- as.factor(youth2010$schule)
youth2010$unique_classroom <- as.factor(youth2010$unique_classroom)

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


#####################
# youth2015
#####################
# kennung, schulart, ortsteil, Klasse
# Klasse geht von 1 bis 11
length(unique(youth2015$kennung))
# 114 distinkte Werte -- Anzahl der Schulklassen?
# Kennung is schulart + ortsteil + klasse and should identify unique classrooms
# schulart + ortsteil should uniquely identify schools

# only exception: documentation looks like there are two Berufs/fachoberschulen
# in 02 (Zentrum-Suedost)

check <- youth2015[youth2015$ortsteil == 2, c("ortsteil", "schulart", "Klasse", "kennung")]
youth2015$school <- paste0(youth2015$ortsteil, youth2015$schulart)
length(unique(youth2015$school))
table(youth2015$school)
youth2015$school[youth2015$school == "NANA"] <- NA
youth2015$school <- as.factor(youth2015$school)

library(lme4)
icc <- lmer(satis ~ 1 + (1|school/kennung), data = youth2015)

summary(icc)
vc <- as.data.frame(VarCorr(icc))

school_var <- vc[vc$grp == "school", "vcov"]
classroom_var <- vc[vc$grp == "kennung:school", "vcov"]
residual_var <- vc[vc$grp == "Residual", "vcov"]

# Calculate ICCs
icc_school <- school_var / (school_var + classroom_var + residual_var) # 0.014
icc_classroom_rel_to_total <- classroom_var / (classroom_var + school_var + residual_var) # 0.014
icc_classroom_inc_school <- (classroom_var + school_var) / (classroom_var + school_var +residual_var) # 0.028

icc_residual <- residual_var / (school_var + classroom_var + residual_var) # 97%

#####################
# youth2023
#####################
table(youth2023$source) # this is almost half online and half paper!
# That seems like a potential issue???
youth2023$factor_source <- as.factor(youth2023$source)
table(youth2023$gender)

mode <- lm(satis ~ as.factor(gender)*factor_source, dat = youth2023[youth2023$gender != "diverse",]) 
summary(mode)
