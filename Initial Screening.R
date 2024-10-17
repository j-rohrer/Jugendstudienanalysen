######################
# Initial screening
######################


######################
# Jugendstudie 2023
######################

load("Files/jugend2023.rds") # I cannot open this file

# Lets check another one
library(haven)
jugend2023 <- read_sav("Files/jugend2023.sav")
View(jugend2023)
names(jugend2023)
# This one works

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

# test