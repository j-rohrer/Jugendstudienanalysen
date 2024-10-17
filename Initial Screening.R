######################
# Initial screening
######################


######################
# Jugendstudie 2023
######################
jugend2023 <- readRDS("Files/jugend2023.rds")

# Wie zufrieden mit Leben insgesamt?
# 1: sehr zufrieden, zufrieden, teils/teils, unzufrieden, sehr unzufrieden
table(jugend2023$f02a)
hist(jugend2023$f02a)

table(jugend2023$geschl)

summary(jugend2023$f02a[jugend2023$geschl == 1]) # maennlich
summary(jugend2023$f02a[jugend2023$geschl == 2]) # weiblich
summary(jugend2023$f02a[jugend2023$geschl == 3]) # divers

prop.table(table(jugend2023$f02a[jugend2023$geschl == 1])) # maennlich
prop.table(table(jugend2023$f02a[jugend2023$geschl == 2])) # weiblich
prop.table(table(jugend2023$f02a)) # alle

summary(jugend2023$wph) # standardgewicht
sum(jugend2023$wph) # not normalized it seems

# weighted frequency table
weighted_freq <- tapply(jugend2023$wph, jugend2023$f02a, sum)
rel_weighted_freq <- weighted_freq / sum(jugend2023$wph)
rel_weighted_freq

# weights dont seem to make a big difference here

######################
# Jugendstudie 2010
######################
jugend2010 <- readRDS("Files/umfj2010.rds")
schul2010 <- readRDS("Files/umfschule2010.rds") # Whats the difference here

erwerb2010 <- readRDS("Files/umfj2010_erwerbverlauf.rds")

# Wie zufrieden zur Zeit
table(jugend2010$j02a)

table(jugend2010$geschl) # 1 maennlich, 2 weiblich

prop.table(table(jugend2010$j02a[jugend2010$geschl == 1])) # maennlich
prop.table(table(jugend2010$j02a[jugend2010$geschl == 2])) # weiblich

mean(jugend2010$j02a[jugend2010$geschl == 1], na.rm = TRUE) # maennlich, 2010
mean(jugend2010$j02a[jugend2010$geschl == 2], na.rm = TRUE) # weiblich, 2010

mean(jugend2023$f02a[jugend2023$geschl == 1], na.rm = TRUE) # maennlich, 2010
mean(jugend2023$f02a[jugend2023$geschl == 2], na.rm = TRUE) # weiblich, 2010

sd(jugend2010$j02a, na.rm = TRUE) # 0.87
sd(jugend2023$f02a, na.rm = TRUE) # 0.85

# Diff girls
(mean(jugend2010$j02a[jugend2010$geschl == 2], na.rm = TRUE) - mean(jugend2023$f02a[jugend2023$geschl == 2], na.rm = TRUE))/
  0.86
# - 0.3

# Diff boys
(mean(jugend2010$j02a[jugend2010$geschl == 1], na.rm = TRUE) - mean(jugend2023$f02a[jugend2023$geschl == 1], na.rm = TRUE))/
  0.86
# 0.13