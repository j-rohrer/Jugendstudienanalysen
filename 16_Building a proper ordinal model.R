#####################################
# Building a proper ordinal model 
#####################################

load("prep.RData")
library(brms)
library(haven)
str(combined$satis)

combined$satis <- as.numeric(combined$satis)
combined$year <- as.factor(combined$year)

ordinal_model <- brm(
  formula = satis ~ year*gender, 
  data = combined,
  family = cumulative("probit"),
  cores = 4
)

summary(ordinal_model)

############################################
# Model with explanation
############################################

combined$german_at_home <- as.factor(combined$german_at_home)

explanation <- brm(
  formula = satis ~  year*gender*german_at_home + gender*source, 
  data = combined,
  family = cumulative("probit"),
  cores = 4
)

summary(explanation)

save.image("ordinal.RData")

