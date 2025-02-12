####################
# What about differences in assessment mode?
####################

# Clean-up: Move variable generation into the first file


# Changes in assessment mode could possibly explain away the effect
# coefficient reduced from 0.19 to 0.12 and no longer statistically significant
# online, girls may be slightly unhappier

load("prep.RData")

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

# no wait, doesnt this actually mean that only girls are unhappier online
mean(youth2023$satis[youth2023$gender == "male" & youth2023$source == "paper"], na.rm = TRUE)
mean(youth2023$satis[youth2023$gender == "male" & youth2023$source == "online"], na.rm = TRUE)
# difference of 0.02

mean(youth2023$satis[youth2023$gender == "female" & youth2023$source == "paper"], na.rm = TRUE)
mean(youth2023$satis[youth2023$gender == "female" & youth2023$source == "online"], na.rm = TRUE)
# difference of 0.11

####################
# can we squeeze this into one model
####################
simple_model_mode_control <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
# now we still have the interaction

simple_model_mode_control <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)
# now we get the non-significant coefficient of 0.12


####################
# Look at the other outcomes as well
####################

# Money: no interaction either way
simple_model_mode_control <- lm(scale(satis_money) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
simple_model_mode_control <- lm(scale(satis_money) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)

# Friends: interaction, and its still highly significant actually
simple_model_mode_control <- lm(scale(satis_friends) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
simple_model_mode_control <- lm(scale(satis_friends) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)

# Mom: no interaction either way
simple_model_mode_control <- lm(scale(satis_mom) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
simple_model_mode_control <- lm(scale(satis_mom) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)

# Dad: barely significant, disappears
simple_model_mode_control <- lm(scale(satis_dad) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
simple_model_mode_control <- lm(scale(satis_dad) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)

# Leisure: diminishes in magnitude, barely significant
simple_model_mode_control <- lm(scale(satis_leisure) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
simple_model_mode_control <- lm(scale(satis_leisure) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)

# Dwelling: remains, .03
simple_model_mode_control <- lm(scale(satis_dwell) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
simple_model_mode_control <- lm(scale(satis_dwell) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)

# not much going on
simple_model_mode_control <- lm(scale(satis_grades) ~ as.factor(year)*as.factor(gender) + as.factor(source), data = combined, weights = wph)
summary(simple_model_mode_control)
simple_model_mode_control <- lm(scale(satis_grades) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender), data = combined, weights = wph)
summary(simple_model_mode_control)



####################
# What is going on here
####################

# one interpretation: girls are just unhappier on tablets
# but alternative: maybe the schools that have tablets are selectively different?
# not sure how that would create the pattern though!

# this gets us back to the question: is there any good evidence that the gender differences
# vary substantially between classrooms? If not, this rules out more substantive alternative
# explanations

# So lets look at whether the gender differences vary between classroom

library(lme4)
library(lmerTest)
library(haven)

# 2010: Some weak evidence for significant variability in the gender diffs
main_gender_2010 <- lmer(satis ~ 1 + gender + (1|unique_classroom), data = combined[combined$year == 2010,])
summary(main_gender_2010)
vary_gender_2010 <- lmer(satis ~ 1 + gender + (1 + gender|unique_classroom), data = combined[combined$year == 2010,])
summary(vary_gender_2010)

anova(main_gender_2010, vary_gender_2010)
# p = .01

# 2015
main_gender_2015 <- lmer(satis ~ 1 + gender + (1|unique_classroom), data = combined[combined$year == 2015,])
summary(main_gender_2015)
vary_gender_2015 <- lmer(satis ~ 1 + gender + (1 + gender|unique_classroom), data = combined[combined$year == 2015,])
summary(vary_gender_2015)

anova(main_gender_2015, vary_gender_2015)
# p = .007

# 2023
main_gender_2023 <- lmer(satis ~ 1 + gender + (1|unique_classroom), data = combined[combined$year == 2023,])
summary(main_gender_2023)
vary_gender_2023 <- lmer(satis ~ 1 + gender + (1 + gender|unique_classroom), data = combined[combined$year == 2023,])
summary(vary_gender_2023)

anova(main_gender_2023, vary_gender_2023)
# diff is no longer significant?

# So there is some weak evidence for variability 

# is the gender difference larger in 2023 in classrooms with a different mode?
vary_gender_2023 <- lmer(satis ~ 1 + gender + (1 + gender|unique_classroom), data = combined[combined$year == 2023,])
summary(vary_gender_2023)

# Now additionally include mode. Does it reduce the variability in the gender effect?
# Current SD is .21

vary_gender_2023 <- lmer(satis ~ 1 + gender + source + (1 + gender|unique_classroom), data = combined[combined$year == 2023,])
summary(vary_gender_2023) # SD is still pretty much .21

# Oh but wait, we need to include the interaction, right
vary_gender_2023 <- lmer(satis ~ 1 + gender*source + (1 + gender|unique_classroom), data = combined[combined$year == 2023,])
summary(vary_gender_2023) # SD is still pretty much .21 tho
# So, the variability in the gender differences between classrooms is not reduced when taking into account 
# the survey mode


####################
# How are the schools with tables different?
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

# Do we have to take into account the schooltype to get rid of the mode effect?

simple_model_mode_control <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(source) + as.factor(schooltype)*as.factor(gender), 
                                data = combined, weights = wph)
summary(simple_model_mode_control)
# now we still have the interaction

simple_model_mode_control <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(source)*as.factor(gender) +  as.factor(schooltype)*as.factor(gender), 
                                data = combined, weights = wph)
summary(simple_model_mode_control)
# now we get the non-significant coefficient of 0.12, so this aint it


####################
# Actually: lets look at the explanandum separately by school type
####################


simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(schooltype), data = combined, weights = wph)
summary(simple_model)

library(marginaleffects)
library(ggplot2)
# generate predictions for combinations of gender and year
pred <- predictions(simple_model,
                    by = c("gender", "year", "schooltype"))

# 1 Mittelschule
ggplot(pred[pred$schooltype == 1,], 
       aes(x = year, group = gender, color = gender, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = .5) +
  scale_x_continuous(breaks = c(2010, 2015, 2023)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_classic() +
  ylab("Mean life satisfaction in SD") +
  xlab("Survey year") +
  ggtitle("Mittelschule")

# 2 Gymnasium
ggplot(pred[pred$schooltype == 2,], 
       aes(x = year, group = gender, color = gender, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = .5) +
  scale_x_continuous(breaks = c(2010, 2015, 2023)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_classic() +
  ylab("Mean life satisfaction in SD") +
  xlab("Survey year") +
  ggtitle("Gymnasium")

# 3 Berufs-/Fachoberschule
ggplot(pred[pred$schooltype == 3,], 
       aes(x = year, group = gender, color = gender, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = .5) +
  scale_x_continuous(breaks = c(2010, 2015, 2023)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_classic() +
  ylab("Mean life satisfaction in SD") +
  xlab("Survey year") +
  ggtitle("Berufs-/Fachoberschule")
# Oh wow these are actually converging what is going on here

# 4 Förderschule
ggplot(pred[pred$schooltype == 4,], 
       aes(x = year, group = gender, color = gender, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = .5) +
  scale_x_continuous(breaks = c(2010, 2015, 2023)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_classic() +
  ylab("Mean life satisfaction in SD") +
  xlab("Survey year") +
  ggtitle("Förderschule")
# This is very different. Those were much happier in 2023, but the sampling also changed

# MAYBE limit to Middle school and Gymnasium from the get go?
# MAYBE also limit to ages 12-18

simple_model_mode_control <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), 
                                data = combined[combined$schooltype < 3,], weights = wph)

pred <- predictions(simple_model_mode_control,
                    by = c("gender", "year"))

ggplot(pred, aes(x = year, group = gender, color = gender, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = .5) +
  scale_x_continuous(breaks = c(2010, 2015, 2023)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_classic() +
  ylab("Mean life satisfaction in SD") +
  xlab("Survey year")
