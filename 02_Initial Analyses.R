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
