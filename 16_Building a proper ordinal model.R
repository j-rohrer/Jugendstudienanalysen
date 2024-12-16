#####################################
# Building a proper ordinal model 
#####################################

load("prep.RData")
library(brms)
library(haven)

combined <- combined[!is.na(combined$satis) & 
                       !is.na(combined$year) & 
                       !is.na(combined$german_at_home) & 
                       !is.na(combined$gender) &
                       !is.na(combined$unique_classroom) &
                       !is.na(combined$source), ]

combined$unique_classroom <- droplevels(combined$unique_classroom)

combined$satis <- as.numeric(combined$satis)
combined$year <- as.factor(combined$year)

ordinal_model <- brm(
  formula = satis ~ year*gender + (1|unique_classroom), 
  data = combined,
  family = cumulative("probit"),
  cores = 4
)


############################################
# Model with explanation
############################################

combined$german_at_home <- as.factor(combined$german_at_home)

explanation <- brm(
  formula = satis ~  year*gender*german_at_home + gender*source + (1|unique_classroom), 
  data = combined,
  family = cumulative("probit"),
  cores = 4
)

summary(ordinal_model)
summary(explanation)


# MAYBE THIS WORKS NOW?
library(marginaleffects)
explanation_slopes <- avg_slopes(explanation, variables = "year", by = c("german_at_home", "gender"))
print(explanation_slopes, nrows = 100)


link_slopes <- avg_slopes(explanation, variables = "year", 
                          by = c("german_at_home", "gender"), 
                          type = "link")
print(link_slopes, nrows = 100)


avg_slopes(explanation, variables = "year", 
           by = c("german_at_home", "gender"), 
           type = "link",
           hypothesis = "b5 - b6 = 0")
# still a significant difference for no german at home
avg_slopes(explanation, variables = "year", 
           by = c("german_at_home", "gender"), 
           type = "link",
           hypothesis = "b7 - b8 = 0")
# if anything, much smaller difference

# Lets compare the differences against each other
avg_slopes(explanation, variables = "year", 
           by = c("german_at_home", "gender"), 
           hypothesis = "b5 - b6 = b7 - b8")
# estimate: 0.0164; 0.000634 vs 0.0363
# I am not entirely sure what is being calculated here


save.image("ordinal.RData")
