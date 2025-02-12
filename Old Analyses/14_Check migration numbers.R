#####################################
# Migration background numbers
#####################################

#####################################
# All schools
#####################################
load("prep.RData")
library(binom)
prop.table(table(combined_all_schooltypes$german_at_home[combined_all_schooltypes$year == 2010])) # 91%
prop.table(table(combined_all_schooltypes$german_at_home[combined_all_schooltypes$year == 2015])) # 91%
prop.table(table(combined_all_schooltypes$german_at_home[combined_all_schooltypes$year == 2023])) # 88%


ewo <- readRDS("Files/Ewo_PLZ_Geschl_Migration.rds")
ewo <- ewo[ewo$Alter >= 12 & ewo$Alter <= 18,]
table(ewo$Zuwanderungshintergrund)

total <- sum(ewo$Anzahl)

sum(ewo$Anzahl[ewo$Zuwanderungshintergrund == "Ausländer"])/total # 12%
sum(ewo$Anzahl[ewo$Zuwanderungshintergrund == "Aussiedler"])/total # 2%
sum(ewo$Anzahl[ewo$Zuwanderungshintergrund == "Einbürgerung"])/total # 6%
sum(ewo$Anzahl[ewo$Zuwanderungshintergrund == "einseitiger elterlicher Zuwanderungshintergrund"])/total # 01%
sum(ewo$Anzahl[ewo$Zuwanderungshintergrund == "ohne (erkennbaren) Zuwanderungshintergrund"])/total # 77%
sum(ewo$Anzahl[ewo$Zuwanderungshintergrund == "persönl. Zuwanderungshintergrund aber Eltern ohne"])/total # 2%

ewo$german_at_home <- NA
ewo$german_at_home[ewo$Zuwanderungshintergrund == "Ausländer"] <- 0
ewo$german_at_home[ewo$Zuwanderungshintergrund == "Einbürgerung"] <- 0

ewo$german_at_home[ewo$Zuwanderungshintergrund == "Aussiedler"] <- 1
ewo$german_at_home[ewo$Zuwanderungshintergrund == "einseitiger elterlicher Zuwanderungshintergrund"] <- 1
ewo$german_at_home[ewo$Zuwanderungshintergrund == "ohne (erkennbaren) Zuwanderungshintergrund"] <- 1
ewo$german_at_home[ewo$Zuwanderungshintergrund == "persönl. Zuwanderungshintergrund aber Eltern ohne"] <- 1

sum(ewo$Anzahl[ewo$Jahr == 2010 & ewo$german_at_home == 1])/sum(ewo$Anzahl[ewo$Jahr == 2010]) # 89.9%
sum(ewo$Anzahl[ewo$Jahr == 2015 & ewo$german_at_home == 1])/sum(ewo$Anzahl[ewo$Jahr == 2015]) # 85%
sum(ewo$Anzahl[ewo$Jahr == 2023 & ewo$german_at_home == 1])/sum(ewo$Anzahl[ewo$Jahr == 2023]) # 75%

# I think we might have to break that down by age group to see what is going on
# Let's do by age, gender, and year -- in the sample versus in the population

# Fold in confidence interval

comp_ewo <- data.frame(matrix(NA, nrow = 7*3, ncol = 10))
names(comp_ewo) <- c("year", "age", "german_girls", "german_girls_lb", "german_girls_ub","german_girls_ewo", 
                     "german_boys", "german_boys_lb", "german_boys_ub", "german_boys_ewo")
comp_ewo$year <- rep(c(2010, 2015, 2023), each = 7)
comp_ewo$age <- rep(12:18, times = 3)
n_temp <- NA
sum_temp <- NA

for (i in 1:nrow(comp_ewo)) {
  print(i)
  # In the data
  # girls, mean
  comp_ewo$german_girls[i] <- mean(combined_all_schooltypes$german_at_home[combined_all_schooltypes$age == comp_ewo$age[i] & 
                                                             combined_all_schooltypes$gender == "female" & 
                                                             combined_all_schooltypes$year == comp_ewo$year[i]], 
                                   na.rm = TRUE)
  # girls, CI
  n_temp <- sum(!is.na(combined_all_schooltypes$german_at_home[combined_all_schooltypes$age == comp_ewo$age[i] & 
                                                 combined_all_schooltypes$gender == "female" & 
                                                 combined_all_schooltypes$year == comp_ewo$year[i]]))
  sum_temp <- sum(combined_all_schooltypes$german_at_home[combined_all_schooltypes$age == comp_ewo$age[i] & 
                                             combined_all_schooltypes$gender == "female" & 
                                             combined_all_schooltypes$year == comp_ewo$year[i]], 
                   na.rm = TRUE)
  comp_ewo[i, c("german_girls_lb", "german_girls_ub")] <- binom.confint(sum_temp, n_temp, conf.level = .95, methods = "wilson")[, c("lower", "upper")]
  
  # boys, mean
  comp_ewo$german_boys[i] <- mean(combined_all_schooltypes$german_at_home[combined_all_schooltypes$age == comp_ewo$age[i] & 
                                                             combined_all_schooltypes$gender == "male" & 
                                                             combined_all_schooltypes$year == comp_ewo$year[i]], 
                                   na.rm = TRUE)
  # boys, ci
  n_temp <- sum(!is.na(combined_all_schooltypes$german_at_home[combined_all_schooltypes$age == comp_ewo$age[i] & 
                                                 combined_all_schooltypes$gender == "male" & 
                                                 combined_all_schooltypes$year == comp_ewo$year[i]]))
  sum_temp <- sum(combined_all_schooltypes$german_at_home[combined_all_schooltypes$age == comp_ewo$age[i] & 
                                            combined_all_schooltypes$gender == "male" & 
                                            combined_all_schooltypes$year == comp_ewo$year[i]], 
                  na.rm = TRUE)
  comp_ewo[i, c("german_boys_lb", "german_boys_ub")] <- binom.confint(sum_temp, n_temp, conf.level = .95, methods = "wilson")[, c("lower", "upper")]
  # In the city data
  comp_ewo$german_girls_ewo[i] <- sum(ewo$Anzahl[ewo$Jahr == comp_ewo$year[i] &
                                                ewo$Alter == comp_ewo$age[i] &
                                                ewo$Geschlecht == "w" &
                                                ewo$german_at_home == 1])/ # number of german speaking girls of that age in that year
    sum(ewo$Anzahl[ewo$Jahr == comp_ewo$year[i] &
                     ewo$Alter == comp_ewo$age[i] &
                     ewo$Geschlecht == "w"]) # divided by total number of girls of that age in that year
  comp_ewo$german_boys_ewo[i] <- sum(ewo$Anzahl[ewo$Jahr == comp_ewo$year[i] &
                                                   ewo$Alter == comp_ewo$age[i] &
                                                   ewo$Geschlecht == "m" &
                                                   ewo$german_at_home == 1])/ # number of german speaking boys of that age in that year
    sum(ewo$Anzahl[ewo$Jahr == comp_ewo$year[i] &
                     ewo$Alter == comp_ewo$age[i] &
                     ewo$Geschlecht == "m"]) # divided by total number of boys of that age in that year
}

# Lets plot the comparison
# Dont forget: 2015 is a different type of assessment in the sample

library(ggplot2)
ggplot(data = comp_ewo[comp_ewo$year == 2010,], aes(x = age)) +
  geom_point(aes(y = german_girls)) +
  geom_line(aes(y = german_girls)) +
  geom_ribbon(aes(ymin = german_girls_lb, ymax = german_girls_ub), alpha = .2) +
  geom_point(aes(y = german_girls_ewo), color = "red") +
  geom_line(aes(y = german_girls_ewo), color = "red") +
  theme_minimal() +
  coord_cartesian(ylim = c(.5, 1)) +
  ggtitle("girls, 2010")

ggplot(data = comp_ewo[comp_ewo$year == 2010,], aes(x = age)) +
  geom_point(aes(y = german_boys)) +
  geom_line(aes(y = german_boys)) +
  geom_ribbon(aes(ymin = german_boys_lb, ymax = german_boys_ub), alpha = .2) +
  geom_point(aes(y = german_boys_ewo), color = "red") +
  geom_line(aes(y = german_boys_ewo), color = "red") +
  theme_minimal() +
  coord_cartesian(ylim = c(.5, 1)) +
  ggtitle("boys, 2010")

ggplot(data = comp_ewo[comp_ewo$year == 2015,], aes(x = age)) +
  geom_point(aes(y = german_girls)) +
  geom_line(aes(y = german_girls)) +
  geom_ribbon(aes(ymin = german_girls_lb, ymax = german_girls_ub), alpha = .2) +
  geom_point(aes(y = german_girls_ewo), color = "red") +
  geom_line(aes(y = german_girls_ewo), color = "red") +
  theme_minimal() +
  coord_cartesian(ylim = c(.5, 1)) +
  ggtitle("girls, 2015")

ggplot(data = comp_ewo[comp_ewo$year == 2015,], aes(x = age)) +
  geom_point(aes(y = german_boys)) +
  geom_line(aes(y = german_boys)) +
  geom_ribbon(aes(ymin = german_boys_lb, ymax = german_boys_ub), alpha = .2) +
  geom_point(aes(y = german_boys_ewo), color = "red") +
  geom_line(aes(y = german_boys_ewo), color = "red") +
  theme_minimal() +
  coord_cartesian(ylim = c(.5, 1)) +
  ggtitle("boys, 2015")

ggplot(data = comp_ewo[comp_ewo$year == 2023,], aes(x = age)) +
  geom_point(aes(y = german_girls)) +
  geom_line(aes(y = german_girls)) +
  geom_ribbon(aes(ymin = german_girls_lb, ymax = german_girls_ub), alpha = .2) +
  geom_point(aes(y = german_girls_ewo), color = "red") +
  geom_line(aes(y = german_girls_ewo), color = "red") +
  theme_minimal() +
  coord_cartesian(ylim = c(.5, 1)) +
  ggtitle("girls, 2023")

ggplot(data = comp_ewo[comp_ewo$year == 2023,], aes(x = age)) +
  geom_point(aes(y = german_boys)) +
  geom_line(aes(y = german_boys)) +
  geom_ribbon(aes(ymin = german_boys_lb, ymax = german_boys_ub), alpha = .2) +
  geom_point(aes(y = german_boys_ewo), color = "red") +
  geom_line(aes(y = german_boys_ewo), color = "red") +
  theme_minimal() +
  coord_cartesian(ylim = c(.5, 1)) +
  ggtitle("boys, 2023")

round(prop.table(table(youth2010$s23aa)),2)
round(prop.table(table(youth2010$s23ba)),2)
round(prop.table(table(youth2010$s23ca)),2)



# Compare precise migration background
prop.table(table(youth2010$migback[youth2010$migback != "none"]))
prop.table(table(youth2023$migback[youth2023$migback != "none"]))


prop.table(table(youth2023$staat_geb_person)) # self
prop.table(table(youth2010$s23ad, useNA = "always"))

prop.table(table(youth2023$staat_geb_vater)) # father
prop.table(table(youth2010$s23bd, useNA = "always"))

prop.table(table(youth2023$staat_geb_mutter)) # mother
prop.table(table(youth2010$s23cd, useNA = "always"))
