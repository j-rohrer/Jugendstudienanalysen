####################
# What about differences in assessment mode?
####################

# Clean-up: Move variable generation into the first file


# Changes in assessment mode could possibly explain away the effect
# coefficient reduced from 0.19 to 0.12 and no longer statistically significant
# online, girls may be slightly unhappier

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
          "satis_leisure", "satis_dwell", "satis_grades", "source")
combined <- rbind(youth2010[, vars], youth2015[, vars], youth2023[, vars])

table(youth2010$gender)
table(youth2015$gender)
table(youth2023$gender)
# Reporting a different gender was not possible in 2010 and 2015
# We thus cannot compare the gender diverse students in 2023 to gender diverse students in earlier years
# Which is why we unfortunately have to exclude them for the present purposes
combined$gender[combined$gender == "diverse"] <- NA

simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined, weights = wph)
summary(simple_model)

####################
# Limit to pen and paper mode
####################
table(combined$source)

simple_model_paper <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined[combined$source == "paper",], weights = wph)
summary(simple_model_paper)

# Okay: coefficient no longer significant but still 0.12 after accounting for paper mode

####################
# Find out how mode affects outcome in 2023
####################

mode_2023 <- lm(scale(satis) ~ as.factor(gender)*as.factor(source), data = youth2023[youth2023$gender != "diverse",], weights = wph)
summary(mode_2023)
# Higher scores for boys
# Higher scores on paper by 0.12
# Slight negative interaction possible? 
# Male have a weaker advantage on paper -- stronger advantage online

####################
# can we squeeze this into one model
####################
simple_model_mode_control <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
# now we still have the interaction

simple_model_mode_control <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)
# now we get the non-significant coefficient of 0.12