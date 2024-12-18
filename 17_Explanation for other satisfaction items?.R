#####################################
# What does our explanation do for the other satisfaction items?
#####################################

load("prep.RData")

# check where we do have a significant interaction
# friends
# father
# leisure
# dwelling


standard <- lm(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home)*as.factor(source)*as.factor(schooltype)*age, 
              data = combined, weights = wph)

avg_slopes(standard, variables = "year", by = c("german_at_home", "gender"))

avg_slopes(standard, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")

avg_slopes(standard, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")



######################
# friends
######################

friends <- lm(scale(satis_friends) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + 
                     as.factor(gender)*as.factor(source), 
                   data = combined, weights = wph)

avg_slopes(friends, variables = "year", by = c("german_at_home", "gender"))

avg_slopes(friends, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")

avg_slopes(friends, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")

######################
# dad
######################

dad <- lm(scale(satis_dad) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + 
                as.factor(gender)*as.factor(source), 
              data = combined, weights = wph)

avg_slopes(dad, variables = "year", by = c("german_at_home", "gender"))

avg_slopes(dad, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")

avg_slopes(dad, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")

######################
# leisure
######################

leisure <- lm(scale(satis_leisure) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + 
            as.factor(gender)*as.factor(source), 
          data = combined, weights = wph)

avg_slopes(leisure, variables = "year", by = c("german_at_home", "gender"))

avg_slopes(leisure, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")

avg_slopes(leisure, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")

######################
# dwell
######################

dwell <- lm(scale(satis_dwell) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + 
                as.factor(gender)*as.factor(source), 
              data = combined, weights = wph)

avg_slopes(dwell, variables = "year", by = c("german_at_home", "gender"))

avg_slopes(dwell, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")

avg_slopes(dwell, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")