######################
# Initial analyses
######################

# remove the incomparable 2010 youth survey of older respondents
# the school survey is the one comparable to the other youth surveys

rm(jugend2010)
# Extract: satisfaction items, migback
###########
# 2010
###########
# gender
schul2010$geschl
schul2010$gender <- NA
schul2010$gender[schul2010$geschl == 1] <- "male"
schul2010$gender[schul2010$geschl == 2] <- "female"
schul2010$gender[schul2010$geschl == 3] <- "diverse"
table(schul2010$gender, schul2010$geschl)

# age
table(schul2010$alter)
schul2010$age <- schul2010$alter

# year
schul2010$year <- 2010

# migback
# self born in Germany? if no, which country
table(schul2010$s23ad, schul2010$s23aa, useNA = "always")

# father born in Germany? if no, which country
table(schul2010$s23bd, schul2010$s23ba, useNA = "always")

# mother born in Germany? if no, which country
table(schul2010$s23cd, schul2010$s23ca, useNA = "always")

# always lived in Germany?
table(schul2010$s24a, useNA = "always")
# in Germany since age...
table(schul2010$s24b, useNA = "always")

schul2010$migback <- NA
# Self, father, mother born in Germany
schul2010$migback[!is.na(schul2010$s23ad) & !is.na(schul2010$s23bd) & !is.na(schul2010$s23cd)] <- "none"
# Self born in Germany, father mother not born in Germany
schul2010$migback[!is.na(schul2010$s23ad) & !is.na(schul2010$s23ba) & !is.na(schul2010$s23ca)] <- "both"
# Self born in Germany, father not born in Germany, mother born in Germany
schul2010$migback[!is.na(schul2010$s23ad) & !is.na(schul2010$s23ba) & !is.na(schul2010$s23cd)] <- "father"
# Self born in Germany, father born in Germany, mother not born in Germany
schul2010$migback[!is.na(schul2010$s23ad) & !is.na(schul2010$s23bd) & !is.na(schul2010$s23ca)] <- "mother"
# Self not born in Germany
schul2010$migback[!is.na(schul2010$s23aa)] <- "self"

table(schul2010$migback, useNA = "always")

# If both parents in Germany, assume self is also born in germany
schul2010$migback[!is.na(schul2010$s23bd) & !is.na(schul2010$s23cd)] <- "none"

# These cannot be coded
check <- schul2010[is.na(schul2010$migback),]
View(check)

# ASSUMED language at home
schul2010$german_at_home <- 1
schul2010$german_at_home[schul2010$migback == "self"|schul2010$migback == "both"] <- 0
table(schul2010$german_at_home)

# Satisfaction items
schul2010$satis <- 6 - schul2010$s02a
schul2010$satis_money <- 6 - schul2010$s02b
schul2010$satis_friends <- 6 - schul2010$s02c
schul2010$satis_mom <- 6 - schul2010$s02d
schul2010$satis_dad <- 6 - schul2010$s02e
schul2010$satis_leisure <- 6 - schul2010$s02g
schul2010$satis_dwell <- 6 - schul2010$s02h
schul2010$satis_grades <- 6 - schul2010$s02f


###########
# 2015
###########
# gender
jugend2015$geschlecht
jugend2015$gender <- NA
jugend2015$gender[jugend2015$geschlecht == 1] <- "male"
jugend2015$gender[jugend2015$geschlecht == 2] <- "female"
jugend2015$gender[jugend2015$geschlecht == 3] <- "diverse"
table(jugend2015$gender, jugend2015$geschlecht)

# age
table(jugend2015$alter)
jugend2015$age <- jugend2015$alter

# year
jugend2015$year <- 2015

# migback
# NO MORE MIGBACK IN 2015 IT SEEMS
# Only information is language spoken at home
table(jugend2015$f19)

jugend2015$german_at_home <- ifelse(jugend2015$f19 == 1, 1, 0)
table(jugend2015$german_at_home)

# Satisfaction items
jugend2015$satis <- 6 - jugend2015$f02a
jugend2015$satis_money <- 6 - jugend2015$f02b
jugend2015$satis_friends <- 6 - jugend2015$f02c
jugend2015$satis_mom <- 6 - jugend2015$f02d
jugend2015$satis_dad <- 6 - jugend2015$f02e
jugend2015$satis_leisure <- 6 - jugend2015$f02g
jugend2015$satis_dwell <- 6 - jugend2015$f02h
jugend2015$satis_grades <- 6 - jugend2015$f02f

jugend2015$migback <- NA

###########
# 2023
###########
# gender
jugend2023$geschl
jugend2023$gender <- NA
jugend2023$gender[jugend2023$geschl == 1] <- "male"
jugend2023$gender[jugend2023$geschl == 2] <- "female"
jugend2023$gender[jugend2023$geschl == 3] <- "diverse"
table(jugend2023$gender, jugend2023$geschl)

# age
table(jugend2023$alter)
jugend2023$age <- jugend2023$alter

# year
jugend2023$year <- 2023

# migback

# born in Germany or elsewhere
table(jugend2023$staat_geb_person) # self
table(jugend2023$staat_geb_vater) # father
table(jugend2023$staat_geb_mutter) # mother

jugend2023$migback <- NA
# Self, father, mother born in Germany
jugend2023$migback[jugend2023$staat_geb_person == 1 & jugend2023$staat_geb_vater == 1 & jugend2023$staat_geb_mutter == 1] <- "none"
# Self born in Germany, father mother not born in Germany
jugend2023$migback[jugend2023$staat_geb_person == 1 & jugend2023$staat_geb_vater == 2 & jugend2023$staat_geb_mutter == 2] <- "both"
# Self born in Germany, father not born in Germany, mother born in Germany
jugend2023$migback[jugend2023$staat_geb_person == 1 & jugend2023$staat_geb_vater == 2 & jugend2023$staat_geb_mutter == 1] <- "father"
# Self born in Germany, father born in Germany, mother not born in Germany
jugend2023$migback[jugend2023$staat_geb_person == 1 & jugend2023$staat_geb_vater == 1 & jugend2023$staat_geb_mutter == 2] <- "mother"
# Self not born in Germany
jugend2023$migback[jugend2023$staat_geb_person == 2] <- "self"

table(jugend2023$migback, useNA = "always")

# If both parents in Germany, assume self is also born in germany
jugend2023$migback[jugend2023$staat_geb_vater == 1 & jugend2023$staat_geb_mutter == 1] <- "none"

# These cannot be coded
check <- jugend2023[is.na(jugend2023$migback),]
check[, c("staat_geb_person", "staat_geb_vater", "staat_geb_mutter")]

# ASSUMED language at home
jugend2023$german_at_home <- 1
jugend2023$german_at_home[jugend2023$migback == "self"|jugend2023$migback == "both"] <- 0
table(jugend2023$german_at_home)

prop.table(table(schul2010$migback, useNA = "always"))
prop.table(table(jugend2023$migback, useNA = "always"))

prop.table(table(schul2010$german_at_home))
prop.table(table(jugend2015$german_at_home))
prop.table(table(jugend2023$german_at_home))

# Satisfaction items
jugend2023$satis <- 6 - jugend2023$f02a
jugend2023$satis_money <- 6 - jugend2023$f02b
jugend2023$satis_friends <- 6 - jugend2023$f02c
jugend2023$satis_mom <- 6 - jugend2023$f02d
jugend2023$satis_dad <- 6 - jugend2023$f02e
jugend2023$satis_leisure <- 6 - jugend2023$f02h
jugend2023$satis_dwell <- 6 - jugend2023$f02i
jugend2023$satis_grades <- as.numeric(6 - jugend2023$f02f)

###########
# combined
###########

vars <- c("gender", "age", "year", "migback", "german_at_home", "satis", "satis_money", "satis_friends",
          "satis_mom", "satis_dad", "satis_leisure", "satis_dwell", "satis_grades")
dat2010 <- schul2010[, vars]
dat2015 <- jugend2015[, vars]
dat2023 <- jugend2023[, vars]

combined <- rbind(dat2010, dat2015, dat2023)
combined$year

fixed <- lm(scale(satis) ~ as.factor(year), data = combined)
summary(fixed)

fixed <- lm(scale(satis) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
summary(fixed)

# Take a look at the various satisfaction variables


fixed <- lm(scale(satis_money) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
summary(fixed) # no interaction

fixed <- lm(scale(satis_friends) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
summary(fixed) # yes interaction!

fixed <- lm(scale(satis_mom) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
summary(fixed) # no interaction

fixed <- lm(scale(satis_dad) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
summary(fixed) # no interaction

fixed <- lm(scale(satis_leisure) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
summary(fixed) # yes interaction!

fixed <- lm(scale(satis_dwell) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
summary(fixed) # yes interaction

# Also consider: age, migback

######################
# Do plot for these
######################
library(marginaleffects)
library(ggplot2)

# satis
mod_satis <- lm(scale(satis) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis <- predictions(mod_satis,
                          newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.4, +0.4)) +
  theme_classic() +
  ylab("Satisfaction with life")
ggsave("Plots/mean_satis.png", width = 4, height = 3)


# satis_money
mod_satis_money <- lm(scale(satis_money) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_money <- predictions(mod_satis_money,
                          newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_money, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.4, +0.4)) +
  theme_classic() +
  ylab("Financial satisfaction")
ggsave("Plots/mean_satis_money.png", width = 4, height = 3)

# satis_friends
mod_satis_friends <- lm(scale(satis_friends) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_friends <- predictions(mod_satis_friends,
                          newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_friends, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.4, +0.4)) +
  theme_classic() +
  ylab("Satisfaction with friends")
ggsave("Plots/mean_satis_friends.png", width = 4, height = 3)

# satis_mom
mod_satis_mom <- lm(scale(satis_mom) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_mom <- predictions(mod_satis_mom,
                          newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_mom, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.4, +0.4)) +
  theme_classic() +
  ylab("Satisfaction with mom")
ggsave("Plots/mean_satis_mom.png", width = 4, height = 3)

# satis_dad
mod_satis_dad <- lm(scale(satis_dad) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_dad <- predictions(mod_satis_dad,
                          newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_dad, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.4, +0.4)) +
  theme_classic() +
  ylab("Satisfaction with dad")
ggsave("Plots/mean_satis_dad.png", width = 4, height = 3)


# satis_leisure
mod_satis_leisure <- lm(scale(satis_leisure) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_leisure <- predictions(mod_satis_leisure,
                          newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_leisure, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.4, +0.4)) +
  theme_classic() +
  ylab("Satisfaction with leisure")
ggsave("Plots/mean_satis_leisure.png", width = 4, height = 3)


# satis_dwell
mod_satis_dwell <- lm(scale(satis_dwell) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_dwell <- predictions(mod_satis_dwell,
                          newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_dwell, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.4, +0.4)) +
  theme_classic() +
  ylab("Satisfaction with dwelling")
ggsave("Plots/mean_satis_dwell.png", width = 4, height = 3)

# satis_grades
mod_satis_grades <- lm(scale(satis_grades) ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_grades <- predictions(mod_satis_grades,
                                newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_grades, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.4, +0.4)) +
  theme_classic() +
  ylab("Satisfaction with grades")
ggsave("Plots/mean_satis_grades.png", width = 4, height = 3)

######################
# Do plot for these unstandardized
######################
library(marginaleffects)
library(ggplot2)

# satis
mod_satis <- lm(satis ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis <- predictions(mod_satis,
                          newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(3, 5)) +
  theme_classic() +
  ylab("Satisfaction with life")
ggsave("Plots/raw_mean_satis.png", width = 4, height = 3)


# satis_money
mod_satis_money <- lm(satis_money ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_money <- predictions(mod_satis_money,
                                newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_money, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(3, 5)) +
  theme_classic() +
  ylab("Financial satisfaction")
ggsave("Plots/raw_mean_satis_money.png", width = 4, height = 3)

# satis_friends
mod_satis_friends <- lm(satis_friends ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_friends <- predictions(mod_satis_friends,
                                  newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_friends, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(3, 5)) +
  theme_classic() +
  ylab("Satisfaction with friends")
ggsave("Plots/raw_mean_satis_friends.png", width = 4, height = 3)

# satis_mom
mod_satis_mom <- lm(satis_mom ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_mom <- predictions(mod_satis_mom,
                              newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_mom, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(3, 5)) +
  theme_classic() +
  ylab("Satisfaction with mom")
ggsave("Plots/raw_mean_satis_mom.png", width = 4, height = 3)

# satis_dad
mod_satis_dad <- lm(satis_dad ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_dad <- predictions(mod_satis_dad,
                              newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_dad, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(3, 5)) +
  theme_classic() +
  ylab("Satisfaction with dad")
ggsave("Plots/raw_mean_satis_dad.png", width = 4, height = 3)


# satis_leisure
mod_satis_leisure <- lm(satis_leisure ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_leisure <- predictions(mod_satis_leisure,
                                  newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_leisure, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(3, 5)) +
  theme_classic() +
  ylab("Satisfaction with leisure")
ggsave("Plots/raw_mean_satis_leisure.png", width = 4, height = 3)


# satis_dwell
mod_satis_dwell <- lm(satis_dwell ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_dwell <- predictions(mod_satis_dwell,
                                newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_dwell, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(3, 5)) +
  theme_classic() +
  ylab("Satisfaction with dwelling")
ggsave("Plots/raw_mean_satis_dwell.png", width = 4, height = 3)

# satis_grades
mod_satis_grades <- lm(satis_grades ~ as.factor(year)*gender, data = combined[combined$gender != "diverse" ,])
pred_satis_grades <- predictions(mod_satis_grades,
                                newdata = datagrid(gender = c("male", "female"), year = c(2010, 2015, 2023)))
ggplot(dat = pred_satis_grades, aes(x = year, y = estimate, ymin = conf.low, ymax = conf.high, group = gender, color = gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(3, 5)) +
  theme_classic() +
  ylab("Satisfaction with grades")
ggsave("Plots/raw_mean_satis_grades.png", width = 4, height = 3)

