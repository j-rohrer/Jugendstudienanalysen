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

# HIER WEITER
# generate classrooms afterwards
# based on schooltype, schoolid, year, and age
# DANN: TESTEN

save(syn_dat, file = "synthetic_data")
