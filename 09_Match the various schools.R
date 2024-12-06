################################
# Match the various schools
################################

load("prep.RData")


combined <- combined[!is.na(combined$school_id) & !is.na(combined$year), ]




simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined, weights = wph)
summary(simple_model)


# in the within-model, this very much seems to stay significant
simple_model_fe <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(school_id), data = combined, weights = wph)
summary(simple_model_fe)


# but are we really only using the overlap?
mapping <- read.csv("Mapping_schools.csv", sep = ";")
mapping$count <- mapping$match_2010 + mapping$match_2015 + mapping$match_2023
table(mapping$count)

include <- mapping$school_id[mapping$count == 3]

table(combined$year, combined$school_id)

# only if included in all three years
simple_model_fe <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(school_id), data = combined[combined$school_id %in% include, ], weights = wph)
summary(simple_model_fe)

# OMG it is still significant for only those years

# What if we include source?
simple_model_fe <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(school_id) + as.factor(gender)*as.factor(source), data = combined[combined$school_id %in% include, ], weights = wph)
summary(simple_model_fe)
# No: no longer significant

simple_model_fe <- lm(scale(satis) ~ as.factor(year)*as.factor(gender) + as.factor(school_id) + as.factor(gender)*as.factor(source)*as.factor(year), data = combined[combined$school_id %in% include, ], weights = wph)
summary(simple_model_fe)
# yeah so this makes it disappear still


# So one possible story here is very much: yes, but the girls are just unhappy because of the different assessment mode