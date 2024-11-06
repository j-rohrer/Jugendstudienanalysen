######################
# Let's figure out whether it could be demographics
######################

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

combined <- combined[!is.na(combined$age),]
combined <- combined[combined$age >= 12 & combined$age <= 18,]
# model with age

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

# Project to general population? Project to population of 2010?