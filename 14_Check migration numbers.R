#####################################
# Migration background numbers
#####################################

load("prep.RData")
prop.table(table(combined$german_at_home[combined$year == 2010])) # 90%
prop.table(table(combined$german_at_home[combined$year == 2015])) # 90%
prop.table(table(combined$german_at_home[combined$year == 2023])) # 87%


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


comp_ewo <- data.frame(matrix(NA, nrow = 7*3, ncol = 6))
names(comp_ewo) <- c("year", "age", "german_girls", "german_girls_ewo", "german_boys", "german_boys_ewo")
comp_ewo$year <- rep(c(2010, 2015, 2023), each = 7)
comp_ewo$age <- rep(12:18, times = 3)



# HIER WEITER
for (i in 1:nrow(comp_ewo)) {
  comp_ewo
  
  mean(combined$german_at_home[combined$age == comp_ewo$age[i] & combined$gender == "female" & combined$year == comp_ewo$year[i]], na.rm = TRUE)
  
  comp_ewo$year[i]
  comp_ewo$age[i]
  
}


