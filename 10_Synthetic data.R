######################################
# Generate synthetic data
######################################

load("prep.RData")

names(combined)

# We are only going to synthesize some of these variables


# schooltype, school_id, year are combined into a single variable
combined$merged_var <- with(combined, paste(schooltype, school_id, year, sep = "_"))


keep_vars <- c("merged_var", "gender", "age", "mig_lang", 
               "satis", "satis_money", "satis_friends", "satis_mom",
               "satis_dad", "satis_leisure", "satis_dwell", "satis_grades", 
               "duration_seconds", "source")

library(synthpop)

combined <- combined[, keep_vars]
str(combined)
library(haven)
combined <- zap_formats(combined)

codebook.syn(combined)

combined_syn <- syn(combined, minnumlevels = 100,
                    method = "parametric",
                    polyreg.maxit = 5000)

# Extract the newly generated dataframe
syn_dat <- combined_syn$syn

# Then let's split them again
split_vars <- do.call(rbind, strsplit(syn_dat$merged_var, "_"))
syn_dat$schooltype <- as.numeric(split_vars[, 1])
syn_dat$school_id <- factor(split_vars[, 2])
syn_dat$year <- as.numeric(split_vars[, 3])


# generate classrooms afterwards
# based on schooltype, schoolid, year, and age

# We take students with the same schoolid and same year and put them into a bucket
syn_dat$bucket <- with(syn_dat, paste(school_id, year, sep = "_"))

# then we put the students into grade levels depending on their age
# most likely grade: age minus 6 (plus minus 1)
syn_dat$grade <- NA
for (i in 1:nrow(syn_dat)) {
  syn_dat$grade[i] <- sample(c(rep(syn_dat$age[i] - 6, times = 4), syn_dat$age[i] - 5, syn_dat$age[i] - 7), size = 1)
}


# logical checks
# lowest grade is 7, highest is 12
syn_dat$grade[syn_dat$grade < 7] <- 7
syn_dat$grade[syn_dat$grade > 12] <- 12

# Oberschule only goes until grade 10
syn_dat$grade[syn_dat$grade > 10 & syn_dat$schooltype == 1] <- 10

# A classroom is a combination of a bucket with a grade
syn_dat$unique_classroom <- paste(syn_dat$bucket, syn_dat$grade, sep = "_")


save(syn_dat, file = "synthetic_data")
