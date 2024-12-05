################################
# Match the various schools
################################

# NEXT UP
# INTEGRATE INTO MAIN TEXT

load("prep.RData")

mapping <- read.csv("Mapping_schools.csv", sep = ";")


# 2010
mapping_2010 <- mapping[mapping$Year == 2010,]
names(mapping_2010)[names(mapping_2010) == "Code"] <- "schule"

mapping_2010 <- mapping_2010[, c("schule", "school_id")]
youth2010 <- merge(youth2010, mapping_2010, by = "schule")
# all successfully matched


# 2015
library(haven)
youth2015_school <- read_spss("Files/jugend2015Schule.sav")
mapping_2015 <- mapping[mapping$Year == 2015,]
names(mapping_2015)[names(mapping_2015) == "Code"] <- "schule"

mapping_2015 <- mapping_2015[, c("schule", "school_id")]
youth2015_school <- merge(youth2015_school, mapping_2015, by = "schule", all.x = TRUE)

youth2015_school <- youth2015_school[, c("rawid", "school_id")]
youth2015 <- merge(youth2015, youth2015_school, by = "rawid", all.x = TRUE)
rm(youth2015_school)


# 2023: Here, we do have a proper variable
# in the main dataset, containing all names
mapping_2023 <- mapping[mapping$Year == 2023,]
names(mapping_2023)[names(mapping_2023) == "Name"] <- "schule_amt51"
mapping_2023 <- mapping_2023[, c("schule_amt51", "school_id")]

youth2023 <- merge(youth2023, mapping_2023, by = "schule_amt51", all.x = TRUE)
table(youth2023[is.na(youth2023$school_id), "schule_amt51"])
# Two schools have not been successfully matched due to special characters in their names
# However, these wont be part of the final analyses (one is a Berufsschule,
# the other one a Förderschule), so we can just ignore this issue

