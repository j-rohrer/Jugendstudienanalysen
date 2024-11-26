####################
# Lets see what an ordinal mode has to say
####################

rm(list = ls())
load("prep.RData")
# Equal weights for the other years
youth2010$wph <- 1
youth2015$wph <- 1

# mode variable
youth2010$source <- "paper"
youth2015$source <- "paper"

# Let's get the variables into a single data frame
vars <- c("year", "wph", "schooltype", "gender", "age", "german_at_home", 
          "satis", "satis_money", "satis_friends", "satis_mom", "satis_dad",
          "satis_leisure", "satis_dwell", "satis_grades", "source", "unique_classroom")
combined <- rbind(youth2010[, vars], youth2015[, vars], youth2023[, vars])
combined$gender[combined$gender == "diverse"] <- NA

simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined, weights = wph)
summary(simple_model)

library(brms)
library(haven)
str(combined$satis)

combined$source <- as.factor(combined$source)
combined$satis <- as.numeric(combined$satis)
combined$year <- as.factor(combined$year)
combined$gender <- as.factor(combined$gender)
ordinal_simple <- brm(satis ~ year*gender, 
                      family = cumulative(),
                      data = combined,
                      cores = 4)

ordinal_simple_weighted <- brm(satis|weights(wph) ~ year*gender, 
                      family = cumulative(),
                      data = combined,
                      cores = 4)
summary(ordinal_simple_weighted)


# Now also add mode

mode_ordinal_weighted <- brm(satis|weights(wph) ~ year*gender + source*gender, 
                               family = cumulative(),
                               data = combined,
                               cores = 4)
summary(mode_ordinal_weighted)

# Actually we have ignored the classroom variable so far!
library(lmerTest)
library(lme4)
simple_mlm <- lmer(scale(satis) ~ year*gender + (1 | unique_classroom), data = combined, weights = wph)
summary(simple_mlm)

simple_mlm_mode <- lmer(scale(satis) ~ year*gender + gender*source + (1 | unique_classroom), data = combined, weights = wph)
summary(simple_mlm_mode)
# is no longer significant



# Fold all together

full_model <- brm(satis|weights(wph) ~ year*gender + source*gender + (1|unique_classroom), 
                             family = cumulative(),
                             data = combined,
                             cores = 4)
summary(full_model)

save.image("intermediate_models.RData")
