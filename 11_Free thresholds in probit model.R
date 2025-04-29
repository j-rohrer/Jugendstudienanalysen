##############################################
# Free thresholds in probit model
##############################################

load("prep.RData")
library(marginaleffects)
library(brms)
library(MASS)

# Do in MASS
combined$satis_factor <- as.factor(combined$satis)

# Simple ordered probit model that estimates the means of life satisfaction by gender and survey year
ordinal <- polr(satis_factor ~ as.factor(year) + gender + gender:as.factor(year), 
                method = "probit",
                Hess = TRUE,
                data = combined)

# Loot at coefficients
summary(ordinal)


# Redo in brms
ordinal_b <- brm(satis ~ as.factor(year) + gender + gender:as.factor(year), 
                family = cumulative(probit),
                data = combined,
                cores = 4,
                seed = 1)

# Loot at coefficients
summary(ordinal_b)
saveRDS(ordinal_b, file = "Models/ordinal_brms")

# As expected, the results are virtually identical

# Free thresholds by gender
ordinal_b_free <- brm(satis| thres(gr = gender) ~ as.factor(year) + gender + gender:as.factor(year), 
                      family = cumulative(probit),
                      data = combined,
                      cores = 4,
                      seed = 1)
summary(ordinal_b_free)
saveRDS(ordinal_b_free, file = "Models/ordinal_brms_free_gender")

# unexpectedly, this model works
# now there is huge uncertainties about the thresholds in men and the
# effect of male gender


# Free the thresholds by year
ordinal_b_free_year <- brm(satis| thres(gr = as.factor(year)) ~ as.factor(year) + gender + gender:as.factor(year), 
                 family = cumulative(probit),
                 data = combined,
                 cores = 4,
                 seed = 1)
summary(ordinal_b_free_year)

saveRDS(ordinal_b_free_year, file = "Models/ordinal_brms_free_year")

# this should also work and as expected
# in turn gives me a lot of uncertainty about the effects of year

<<<<<<< HEAD
=======

##############################################
# Do age as a spline
##############################################
>>>>>>> 5b111da34be9a0989cd54fb7b77c2235f2103a13
